
public class FinishEncounter extends Encounter {

	private final static String name = "Finish point";
	private final static String nearbyAlert = "you see a golden beam of light.";

	public FinishEncounter(OpenWorld game, Player player, int northSouth, int eastWest) {
		super(game, player, name, nearbyAlert, northSouth, eastWest);
	}

	@Override
	public void update(Coordinates playerLocation) {
		if (encounterCoordinates.isHere(playerLocation)) {
			System.out.println("You have reached your goal!");
			game.setGameOver(true);
		}
		super.nearbyMessage(playerLocation);
	}

}
