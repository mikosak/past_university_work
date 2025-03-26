
public class WizardEncounter extends Encounter {

	private final static String name = "Wizard";
	private final static String nearbyAlert = "you see an old man with a curious hat.";

	public WizardEncounter(OpenWorld game, Player player, int northSouth, int eastWest) {
		super(game, player, name, nearbyAlert, northSouth, eastWest);
	}

	@Override
	public void update(Coordinates playerLocation) {
		if (encounterCoordinates.isHere(playerLocation)) {
			System.out.println("The Wizard flattens the surrounding areas!");
			game.alertFlattening();
		}
		super.nearbyMessage(playerLocation);
	}

}
