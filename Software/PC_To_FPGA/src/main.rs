#[macro_use]
extern crate bmp;
use bmp::{Pixel, Image};
use std::{thread, time};
use clap::{Arg, App};
use std::fs::File;
use gif::SetParameter;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
extern crate ftdi_embedded_hal as hal;

extern crate embedded_hal;
use embedded_hal::blocking::spi::Transfer;

use crate::hal::x232h::FTx232H;

fn get_bit_at(input: u8, n: u8) -> bool {
    if n < 8 {
        input & (1 << n) != 0
    } else {
        false
    }
}

fn show_bmp(img: &mut Image, spidev: &mut hal::spi::SpiBus, brightness: u32)
{
    let mut image_in_rows: [[u8; 3*64] ; 64] = [[0; 3*64] ; 64];
    let mut row_total: [[u64; 64] ; 24] = [[0; 64] ; 24];
    
    for (x, y) in img.coordinates() {
        let mut pixel_temp: Pixel = img.get_pixel(x, y);
        pixel_temp.r = ((pixel_temp.r as f64 / 255.0).powf(1.5) * 255.0) as u8;
        pixel_temp.g = ((pixel_temp.g as f64 / 255.0).powf(1.5) * 255.0) as u8;
        pixel_temp.b = ((pixel_temp.b as f64 / 255.0).powf(1.5) * 255.0) as u8;
        img.set_pixel(x, y, pixel_temp);
    }
    
    // turn the image from a 64*64 array of RGB pixels to an array of 64 * 192 of u8's 
    // containing a row of red, then green, then blue, then red and so on u8's
    for (x, y) in img.coordinates() {
        let pixel_temp: Pixel = img.get_pixel(x, y);
        image_in_rows[y as usize][(x*3) as usize] = pixel_temp.r;
        image_in_rows[y as usize][((x*3)+1) as usize] = pixel_temp.g;
        image_in_rows[y as usize][((x*3)+2) as usize] = pixel_temp.b;
        
        
        //The display has 8 bit colour per pixel, memory is stored as 64 bit red MSB, 64 bit red msb-1, 64 bit red msb-2 and so on.
        //then green, then blue. So 24 64bit words per row of 64 LEDs. 
        //This code turns the array with u8's per colour into the above.
        for n in 0..8 {
            if get_bit_at(image_in_rows[y as usize][ (x*3) as usize], n)
            {
                row_total[(n) as usize][y as usize] += 1 << x;
            }
            if get_bit_at(image_in_rows[y as usize][ ((x*3)+1) as usize], n)
            {
                row_total[(8+n) as usize][y as usize] += 1 << x;
            }
            if get_bit_at(image_in_rows[y as usize][ ((x*3)+2) as usize], n)
            {
                row_total[(16+n) as usize][y as usize] += 1 << x;
            }
        }
    }
    
    let value_brightness = brightness;
    
    let mut data_to_send: [u8; 8] = [0; 8];
    
    let mut chunk_to_send: [u8; 2*24*8] = [0; 2*24*8];
    let mut address:u16 = 0x4000;
    
    //split 64 bit words into 2 * 32 bit, wrap around the wishbone protocol (0x01, 0x01, address, value) and send over.
    for y in 0..64 {
        
        for n in 0..24 {
            let value_low:u32 = (row_total[n as usize][y as usize] >> 32) as u32;
            let value_high:u32 = row_total[n as usize][y as usize] as u32;
            
            let address_bytes = address.to_be_bytes();
            let value_bytes = value_low.to_be_bytes();
            
            chunk_to_send[(n*16)+0] = 1;
            chunk_to_send[(n*16)+1] = 1;
            
            for x in 0..2 {
                chunk_to_send[(n*16)+(2+x) as usize] = address_bytes[x];
            }
            
            for x in 0..4 {
                chunk_to_send[(n*16)+(4+x) as usize] = value_bytes[x];
            }
            address = address + 1;
            
            let address_bytes = address.to_be_bytes();
            let value_bytes = value_high.to_be_bytes();
            
            chunk_to_send[((n*16)+8)+0] = 1;
            chunk_to_send[((n*16)+8)+1] = 1;
    
            for x in 0..2 {
                chunk_to_send[((n*16)+8)+(2+x) as usize] = address_bytes[x];
            }
            
            for x in 0..4 {
                chunk_to_send[((n*16)+8)+(4+x) as usize] = value_bytes[x];
            }
            
            address = address + 1;
        }
        spidev.transfer(&mut chunk_to_send).unwrap();
    }
    
    address = 0;
    let address_bytes = address.to_be_bytes();
    let value_bytes = value_brightness.to_be_bytes();
    
    data_to_send[0] = 1;
    data_to_send[1] = 1;
    
    for n in 0..2 {
        data_to_send[(2+n) as usize] = address_bytes[n];
    }
    
    for n in 0..4 {
        data_to_send[(4+n) as usize] = value_bytes[n];
    }
    spidev.transfer(&mut data_to_send).unwrap();
    
    address = 4;
    let address_bytes = address.to_be_bytes();
    let value_memory:u32 = 1;
    let value_bytes = value_memory.to_be_bytes();
    
    data_to_send[0] = 1;
    data_to_send[1] = 1;
    
    for n in 0..2 {
        data_to_send[(2+n) as usize] = address_bytes[n];
    }
    
    for n in 0..4 {
        data_to_send[(4+n) as usize] = value_bytes[n];
    }
    spidev.transfer(&mut data_to_send).unwrap();
}

fn main() {

	let matches = App::new("image loader")
       .version("0.1")
       .about("Loads an image to rgb matrix")
       .author("Rik")
		.arg(
			Arg::with_name("Image file location")
			.help("Location of the image file to transmit")
			.takes_value(true)
			.short("f")
            .long("file")
			.required(true),
        )
        .arg(
			Arg::with_name("Brightness")
			.help("Brightness of the panel")
			.takes_value(true)
			.short("b")
            .long("brightness")
			.required(true),
        )
        .arg(
			Arg::with_name("Loop")
			.help("1 if a gif should loop forever")
			.takes_value(true)
			.short("l")
            .long("loop")
			.required(false),
        )
       .get_matches(); 
    
    let dev = FTx232H::init(0x0403, 0x6015).unwrap();  //Normally 0403 6014 but that already is my FPGA programmer :)
    let mut spidev = dev.spi(hal::spi::SpiSpeed::CLK_10MHz).unwrap();
    
    let shutdownApplication = Arc::new(AtomicBool::new(false));
    let sharedShutdownApplication = Arc::clone(&shutdownApplication);
    ctrlc::set_handler(move || {
        sharedShutdownApplication.swap(true, Ordering::Relaxed);
    })
    .expect("Error setting Ctrl-C handler");
    
    let mut value_brightness:u32;
    let brightness = matches.value_of("Brightness").unwrap();	
    if let Ok(brigh) = brightness.parse::<u32>() {
        value_brightness = brigh.into();
    } else {
        eprintln!("Error: Invalid brightness '{}' specified", brightness);
        ::std::process::exit(1);
    }
    
    let filename = matches.value_of("Image file location").unwrap();
    println!("file name: {}", filename);
    let tokens:Vec<&str>= filename.split(".").collect();
    if(tokens[1] == "bmp") {
        let mut img = bmp::open(filename).unwrap_or_else(|e| {
            panic!("Failed to open: {}", e);
        });
        
        show_bmp(&mut img.clone(), &mut spidev, value_brightness);

    }
    else if(tokens[1] == "gif") {
        
        let mut loopval:u32 = 0;
        if let Some(o) = matches.value_of("Loop") {
            if let Ok(loopint) = o.parse::<u32>() {
                loopval = loopint.into();
            } else {
                eprintln!("Error: Invalid loop '{}' specified", o);
                ::std::process::exit(1);
            }
        }
        
        loop{
        
            let mut decoder = gif::Decoder::new(File::open(filename).unwrap());
            decoder.set(gif::ColorOutput::RGBA);
            let mut decoder = decoder.read_info().unwrap();
            
            let mut imggif = Image::new(64, 64);
            
            let mut cnt = 0;
            
            
            while let Some(frame) = decoder.read_next_frame().unwrap() {
                if(frame.delay >= 4){
                    thread::sleep(time::Duration::from_millis((10*(frame.delay-3)).into()));
                }
                        
                for y in 0..frame.width {
                    for x in 0..frame.height {
                        let mult = frame.width*4;
                        if(frame.buffer[3+((y*4)+(x*mult)) as usize] == 255) {
                            imggif.set_pixel((frame.left + y).into(), (frame.top + x).into(), px!(frame.buffer[0+((y*4)+(x*mult)) as usize], frame.buffer[1+((y*4)+(x*mult)) as usize], frame.buffer[2+((y*4)+(x*mult)) as usize]));
                        }
                    }
                }
                
                show_bmp(&mut imggif.clone(), &mut spidev, value_brightness);
                
                if(shutdownApplication.load(Ordering::Relaxed) == true){
                    loopval = 0;
                    break;
                }
                    
            }
            
            if(loopval != 1){break;}
        }
        
    }
    println!("Done!");
 
}
