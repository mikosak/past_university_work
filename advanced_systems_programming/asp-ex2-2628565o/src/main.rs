use std::io::{Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::process;
use std::sync::mpsc;
use std::thread;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let hostname = args[1].clone();
    let addr_with_port = format!("{}:80", hostname);

    // DNS resolution
    let addresses = match addr_with_port.to_socket_addrs() {
        Ok(addrs) => addrs.collect::<Vec<_>>(),
        Err(e) => {
            eprintln!("Couldn't resolve {}, {}.", hostname, e);
            process::exit(1);
        }
    };

    let (client_tx, client_rx) = mpsc::channel();
    let mut ip_attempt_channels = Vec::new();
    let mut connection_attempts = Vec::new();

    // Create connection attempt threads
    for _ in &addresses {
        let (ip_tx, ip_rx) = mpsc::channel();
        ip_attempt_channels.push(ip_tx);
        
        let client_tx = client_tx.clone();
        connection_attempts.push(thread::spawn(move || {
            match ip_rx.recv() {
                Ok(addr) => {
                    match TcpStream::connect_timeout(&addr, std::time::Duration::from_secs(5)) {
                        Ok(stream) => {
                            client_tx.send(stream).unwrap();
                        }
                        Err(e) => {
                            eprintln!("Failed to connect to {}, {}.", addr, e);
                        }
                    }
                }
                Err(e) => {
                     eprintln!("Channel closed before receiving address, {}.", e)
                }
            }
        }));
    }

    // Send addresses to connection attempt threads
    for (tx, addr) in ip_attempt_channels.into_iter().zip(addresses.into_iter()) {
        tx.send(addr).unwrap();
    }

    // Connected client thread
    let client_thread = thread::spawn(move || {
        let mut finished = false;
        for mut stream in client_rx {
            if !finished {
                finished = true;
                println!("Connected to client {}. Sending HTTP request.", stream.peer_addr().unwrap());

                let request = format!(
                    "GET / HTTP/1.1\r\n\
                     Host: {}\r\n\
                     Connection: close\r\n\
                     \r\n",
                    hostname
                );

                let mut response = Vec::new();
                match stream.write_all(request.as_bytes()) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("Failed to write to {}, {}. Trying next connection.", stream.peer_addr().unwrap(), e);
                        finished = false;
                        continue;
                    }
                };

                match stream.read_to_end(&mut response) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("Failed to read from {}, {}. Trying next connection.", stream.peer_addr().unwrap(), e);
                        finished = false;
                        continue;
                    }
                };
                

                match String::from_utf8(response) {
                    Ok(s) => {
                        println!("\n{}", s);
                        let _ = stream.shutdown(std::net::Shutdown::Both);
                    },
                    Err(_) => {
                        eprintln!("{} returned a response that contains invalid UTF-8.", hostname);
                        let _ = stream.shutdown(std::net::Shutdown::Both);
                        process::exit(1);
                    }
                }
            } else {
                let _ = stream.shutdown(std::net::Shutdown::Both);
            }
        }
    });

    // Wait for all connection attempts to finish
    for attempt in connection_attempts {
        attempt.join().unwrap();
    }

    // Wait for the connected client thread to finish
    drop(client_tx);
    client_thread.join().unwrap();
}