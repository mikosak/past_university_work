import java.util.Random;

public class PotionEncounter extends Encounter {

	private final static String name = "Magical potion";
	private final static String nearbyAlert = "you see an odd little vial on the floor.";

	public PotionEncounter(OpenWorld game, Player player, int northSouth, int eastWest) {
		super(game, player, name, nearbyAlert, northSouth, eastWest);
	}

	@Override
	public void update(Coordinates playerLocation) {
		if (encounterCoordinates.isHere(playerLocation)) {
			Random rand = new Random();
			int choice = rand.nextInt(3);
			switch (choice) {
			case (0):
				System.out.println("You feel invigorated!");
				player.changeHealth(30);
				break;
			case (1):
				System.out.println("You feel pain spread throughout your body...");
				player.changeHealth(-20);
				break;
			case (2):
				System.out.println("It lets out a pungent smell... you get surrounded by wolves!");
				game.alertWolves();
				break;
			}
		}
		super.nearbyMessage(playerLocation);
	}

}
