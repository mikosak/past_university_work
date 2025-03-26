
public class FlatlandsEncounter extends Encounter {

	private final static String name = "Flatlands";
	private final static String nearbyAlert = "you see boring open planes";

	public FlatlandsEncounter(OpenWorld game, Player player, int northSouth, int eastWest) {
		super(game, player, name, nearbyAlert, northSouth, eastWest);
	}

	@Override
	public void update(Coordinates playerLocation) {
		if (encounterCoordinates.isHere(playerLocation)) {
			System.out.println("Nothing to see here");
		}
		super.nearbyMessage(playerLocation);

	}

}
