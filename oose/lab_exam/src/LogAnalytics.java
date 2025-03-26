
public class LogAnalytics implements PlayerObserver {

	private static LogAnalytics loggerInstance;

	private LogAnalytics(OpenWorld game) {
		game.getPlayerLocation().registerObserver(this);
		System.out.println("Making a connection to the external database");
	}

	public static LogAnalytics getLogger(OpenWorld game) {
		if (loggerInstance == null) {
			loggerInstance = new LogAnalytics(game);
		}
		return loggerInstance;
	}

	@Override
	public void update(Coordinates newCoordinates) {
		logMove(newCoordinates);
	}

	public void logMove(Coordinates newCoordinates) {
		System.out.println("Logging: " + newCoordinates.toString());
	}
}
