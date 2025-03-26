
public class Player {

	private int health;

	public Player(int health) {
		this.health = health;
	}

	public int getHealth() {
		return health;
	}

	public void changeHealth(int amount) {
		if (-amount >= this.health) {
			this.health = 0;
			System.out.println("Player is knocked out! Game over.");
		} else {
			this.health += amount;
		}
	}

}
