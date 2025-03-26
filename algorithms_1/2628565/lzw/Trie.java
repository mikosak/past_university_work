package lzw;

public class Trie {
	private Node root = new Node(Character.MIN_VALUE, null);
	private int nextCode = 0;
	private int codewordLength = 8;

	public Trie() {
		remakeTrie();
	}

	// used to enforce the codeword length limits
	private void remakeTrie() {
		for (int i = 0; i < 1 << codewordLength; i++) {
			insert(String.valueOf((char) i), i);
		}
	}

	private enum Outcomes {
		PRESENT, ABSENT, UNKNOWN
	}

	// sort modified to accept codes
	public Integer search(String w) {
		Outcomes outcome = Outcomes.UNKNOWN;
		int i = 0;
		Node current = root.getChild();

		while (outcome == Outcomes.UNKNOWN) {

			if (current == null) {
				outcome = Outcomes.ABSENT;
			} else if (current.getLetter() == w.charAt(i)) {

				if (i == w.length() - 1) {
					outcome = Outcomes.PRESENT;
				} else {
					current = current.getChild();
					i++;
				}
			} else {
				current = current.getSibling();
			}
		}

		if (outcome != Outcomes.PRESENT || !current.isWord())
			return null;
		else
			return current.getCode();
	}

	public int getNextCode() {
		return nextCode++;
	}

	// insert modified to accept codes
	public void insert(String w, Integer code) {
		int i = 0;
		Node current = root;
		Node next = current.getChild();

		while (i < w.length()) {

			if (next != null && next.getLetter() == w.charAt(i)) {
				current = next;
				next = current.getChild();
				i++;
			} else if (next != null) {
				next = next.getSibling();
			} else {
				Node x = new Node(w.charAt(i), null);
				x.setSibling(current.getChild());
				current.setChild(x);
				current = x;
				next = current.getChild();
				i++;
			}
		}
		current.setWord(true);
		current.setCode(code);
	}

	public void reset() {
		nextCode = 0;
		codewordLength++;
		root = new Node(Character.MIN_VALUE, null);
		remakeTrie();
	}

	public int getCodewordLength() {
		return codewordLength;
	}

}
