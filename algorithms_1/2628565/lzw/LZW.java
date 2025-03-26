package lzw;

public class LZW {
	private Trie dictionary = new Trie();
	private StringBuilder compressedText = new StringBuilder();

	public LZW() {
		// all ASCII characters
		for (int i = 0; i < 128; i++) {
			dictionary.insert(String.valueOf((char) i), i);
		}
	}

	public void compress(String input) {
		String current = "";
		Integer temp = null;

		// while input...
		for (char c : input.toCharArray()) {
			String newString = current + c;
			Integer code = dictionary.search(newString);

			// if code is in the dictionary...
			if (code != null) {
				current = newString;
			} else {
				// if not...
				temp = dictionary.search(current);

				if (temp != null) {
					compressedText.append((char) (int) temp);
				}
				// add it!
				dictionary.insert(newString, dictionary.getNextCode());
				current = String.valueOf(c);
			}

			// check if codeword length has been exceeded
			if (dictionary.getNextCode() == 1 << dictionary.getCodewordLength()) {
				dictionary.reset();
			}
		}

		if (!current.isEmpty()) {
			temp = dictionary.search(current);
			compressedText.append((char) (int) temp);
		}	
		calculateCompression(input);
	}

	private void calculateCompression(String input) {
		int originalSize = input.length() * 8;
		int compressedSize = compressedText.length() * 8;
		float compressionRatio = (float) compressedSize / originalSize;

		System.out.println("Original file length in bits = " + originalSize);
		System.out.println("Compressed file length in bits = " + compressedSize);
		System.out.println("Compression ratio = " + compressionRatio);
	}
}
