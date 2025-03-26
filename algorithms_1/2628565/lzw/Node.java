package lzw;

public class Node {

	private char letter;
	private boolean isWord;
	private Integer code;
	private Node sibling;
	private Node child;

	public Node(char c, Integer code) {
		letter = c;
		isWord = false;
		this.code = code;
		sibling = null;
		child = null;
	}

	public char getLetter() {
		return letter;
	}

	public boolean isWord() {
		return isWord;
	}

	public void setWord(boolean isWord) {
		this.isWord = isWord;
	}

	public Integer getCode() {
		return code;
	}

	public void setCode(Integer code) {
		this.code = code;
	}

	public Node getSibling() {
		return sibling;
	}

	public void setSibling(Node sibling) {
		this.sibling = sibling;
	}

	public Node getChild() {
		return child;
	}

	public void setChild(Node child) {
		this.child = child;
	}

}
