package com.example;
import java.sql.*;
import java.util.Scanner;

public class ProductSearcher {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter the product name");
        String productName = scanner.nextLine();
        scanner.close();

        try (Connection dbConnect = DriverManager.getConnection("jdbc:productdatabase", "admin", "adminpsd")) {
            String sqlStatement = "SELECT * FROM products WHERE product_name = ?";
            try (PreparedStatement searchStatement = dbConnect.prepareStatement(sqlStatement)) {
                searchStatement.setString(1, productName);
                ResultSet searchResult = searchStatement.executeQuery();
                while (searchResult.next()) {
                    System.out.println("Product found: " + searchResult.getString("product_name"));
                }
                System.out.println("SQL query executed: [" + sqlStatement + "]");
            }
        } catch (SQLException sqlErr) {
            sqlErr.printStackTrace();
        }
    }
}