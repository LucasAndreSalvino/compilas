package teste2;

import java.io.FileReader;
import java.io.StringReader;
import java.nio.file.Paths;

public class Main {

    public static void main(String[] args) {

    	System.out.println("print estou aqui");
        System.out.println("");

sd,ljfbajkwbfa;sbfdhbsahçfhbhçsfhgbçshagbshçgçhsbfdçhgbhçdsbfçhgbçhdsbhfçgbçhdbhfsgbçhsdfgçhb
        try {
            Parser p = new Parser(new Lexer(new StringReader(" main;\n" + 
            		"import \"fmt\";\n" + 
            		"func sdbfgbihdsfkgb(5t5tpackage){ \n" + 
            		"    // var ( i, j, k int = +1, -3; n, m string = \"Vase\", \"Pane\"; );\n" + 
            		"    // 8=fkhsabdfhkbsakhdfbhksadbf0;\ndfgdsfgdsfgdsfgsdfg" + 
            		"};")));
            Object result = p.parse().value;
            System.out.println("print estou aqui");
            System.out.println("");
        } catch (Exception e) {
            e.printStackTrace();
        }

    }
}
