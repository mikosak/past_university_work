
public class LakeEncounter extends Encounter {

	private final static String name = "Lake";
	private final static String nearbyAlert = "there is a vast expanse of water.";

	public LakeEncounter(OpenWorld game, Player player, int northSouth, int eastWest) {
		super(game, player, name, nearbyAlert, northSouth, eastWest);
	}

	@Override
	public void update(Coordinates playerLocation) {
		if (encounterCoordinates.isHere(playerLocation)) {
			System.out.println("The water is icy cold!");
			player.changeHealth(-10);
		}
		super.nearbyMessage(playerLocation);
	}

}
