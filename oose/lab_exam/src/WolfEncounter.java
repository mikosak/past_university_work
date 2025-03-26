
public class WolfEncounter extends Encounter {

	private final static String name = "Wolf";
	private final static String nearbyAlert = "you hear distant howling.";

	public WolfEncounter(OpenWorld game, Player player, int northSouth, int eastWest) {
		super(game, player, name, nearbyAlert, northSouth, eastWest);
	}

	@Override
	public void update(Coordinates playerLocation) {
		if (encounterCoordinates.isHere(playerLocation)) {
			System.out.println("You are attacked by wolves!");
			player.changeHealth(-20);
		}
		super.nearbyMessage(playerLocation);
	}

}
