package huffman;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {

	public static void main(String[] args) throws IOException {

		long start = System.currentTimeMillis();
		String inputFileName = args[0];
		String content = new String(Files.readAllBytes(Paths.get(inputFileName))).toLowerCase();
		Tree huffman = new Tree();

		huffman.addText(content);
		huffman.makeTree();

		long end = System.currentTimeMillis();
		System.out.println("Elapsed time: " + (end - start) + " milliseconds");
	}

}
