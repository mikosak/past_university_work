package huffman;

import java.util.HashMap;
import java.util.PriorityQueue;

public class Tree {
	private HashMap<Character, Integer> count = new HashMap<>();
	private PriorityQueue<Node> nodeList = new PriorityQueue<>(new sortByCount());
	private Node root;

	public Tree() {
	}

	public Node getRoot() {
		return root;
	}

	// counts character occurrences into hashmap
	public void addText(String input) {

		for (char ch : input.toCharArray()) {
			if (count.get(ch) == null) {
				count.put(ch, 1);
			} else {
				count.put(ch, count.get(ch) + 1);
			}
		}
	}

	// turns hashmap into a priority queue sorted by frequency
	public void makeTree() {
		nodeList.addAll(
				count.entrySet().stream().map(entry -> new Node(entry.getKey(), entry.getValue(), true)).toList());

		while (nodeList.size() > 1) {

			Node z = new Node(' ', 0, false);
			Node x = nodeList.poll();
			Node y = nodeList.poll();
			z.setLeft(x);
			z.setRight(y);
			z.setCount(x.getCount() + y.getCount());
			nodeList.offer(z);
		}
		root = nodeList.poll();
		calculateCompression();
	}

	private void calculateCompression() {
		int sizeBefore = root.getCount() * 8;
		int sizeAfter = DFScompressedSize(root, 0, 0);
		float compressionRatio = (float) sizeAfter / sizeBefore;

		System.out.println("Original file length in bits = " + sizeBefore);
		System.out.println("Compressed file length in bits = " + sizeAfter);
		System.out.println("Compression ratio = " + compressionRatio);
	}

	// DFS traversal to calculate compressed size
	private int DFScompressedSize(Node node, int depth, int sum) {

		if (node != null) {
			if (node.isLeaf()) {
				sum += (node.getCount() * depth);
			}
			sum = DFScompressedSize(node.getLeft(), depth + 1, sum);
			sum = DFScompressedSize(node.getRight(), depth + 1, sum);
		}
		return sum;
	}
}
