import scala.collection.immutable.List;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import java.awt.event.WindowEvent;

public class Tablero {
    private static int filas;
    private int columnas;
    private int limiteSuperior;
    private int limiteInferior=1;
    private int modo;
    private int dificultad;
    private int vidas=5;
    private int x;
    private int y;
    private static List<Object> matriz;
    JPanel Fondo;
    JTable tablero;
    private JLabel Prueba;

    public Tablero(List<Object> matriz,int filas, int columnas, int limiteSuperior, int limiteInferior, int modo, int dificultad, int vidas, int x, int y) {
        this.filas = filas;
        this.columnas = columnas;
        this.limiteSuperior = limiteSuperior;
        this.limiteInferior = limiteInferior;
        this.modo = modo;
        this.dificultad = dificultad;
        this.vidas = vidas;
        this.x = x;
        this.y = y;
        this.matriz = matriz;
        DefaultTableModel model = new DefaultTableModel(filas, columnas);

        // crea el JTable y establece el modelo
        tablero = new JTable(model);
        actualizarTablero(matriz,0,0);
    }

    public void windowOpened(WindowEvent e) {
        // aquí va la función que se ejecutará cuando se abra la ventana
        tablero.setValueAt("Prueba",0,0);
        Prueba.setText("Prueba");
        tablero.setVisible(true);
    }

    public void actualizarTablero(List<Object> mat,int fil,int col){
        if (fil<filas) {
            if (col==columnas) {
                fil++;
                col=0;
                actualizarTablero(mat,fil,col);
            }
            else {
                tablero.setValueAt(mat.head().toString(), fil, col);
                actualizarTablero((List<Object>) mat.tail(), fil, col+1);
            }
        }
    }


}
