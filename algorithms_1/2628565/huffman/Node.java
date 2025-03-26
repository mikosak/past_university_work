package huffman;

import java.util.Comparator;

public class Node {

	private boolean isLeaf;
	private char letter;
	private int count;
	private Node left;
	private Node right;

	public Node(char c, int number, boolean isLeaf) {
		this.isLeaf = isLeaf;
		letter = c;
		count = number;
		left = null;
		right = null;
	}

	public char getLetter() {
		return letter;
	}

	public int getCount() {
		return count;
	}

	public boolean isLeaf() {
		return isLeaf;
	}

	public void setCount(int count) {
		this.count = count;
	}

	public Node getLeft() {
		return left;
	}

	public void setLeft(Node left) {
		this.left = left;
	}

	public Node getRight() {
		return right;
	}

	public void setRight(Node right) {
		this.right = right;
	}

}

class sortByCount implements Comparator<Node> {
	public int compare(Node a, Node b) {
		return Integer.compare(a.getCount(), b.getCount());
	}
}
