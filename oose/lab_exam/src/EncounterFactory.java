
public class EncounterFactory {

	public EncounterFactory() {
	}

	public Encounter createEncounter(int choice, OpenWorld game, Player player, int northSouth, int eastWest) {
		if (choice == 0) {
			return new FlatlandsEncounter(game, player, northSouth, eastWest);
		}
		if (choice == 1) {
			return new FinishEncounter(game, player, northSouth, eastWest);
		}
		if (choice == 2) {
			return new WolfEncounter(game, player, northSouth, eastWest);
		}
		if (choice == 3) {
			return new LakeEncounter(game, player, northSouth, eastWest);
		}
		if (choice == 4) {
			return new WizardEncounter(game, player, northSouth, eastWest);
		}
		if (choice == 5) {
			return new PotionEncounter(game, player, northSouth, eastWest);
		} else {
			return null;
		}
	}

}
