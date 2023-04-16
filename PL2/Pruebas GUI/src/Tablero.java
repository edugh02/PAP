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
    private Main clasePrincipal;
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
                ImageIcon imagen = obtenerImagenDesdeIndice(mat, fil, col);
                tablero.setValueAt(imagen, fil, col);
                System.out.println("llega aqui");
                actualizarTablero((List<Object>) mat.tail(), fil, col+1);
            }
        }
    }


    private ImageIcon obtenerImagenDesdeIndice(List<Object> mat, int fil,int col) {
        // Lógica para obtener la imagen correspondiente al índice
        // por ejemplo, con un switch:
        int indice= fil*columnas+col;
        Object elem= clasePrincipal.getElem(mat,indice);
        switch (elem.toString()) {
            case "1":
                return new ImageIcon("imagenes/Azul.jpg");
            case "2":
                return new ImageIcon("imagenes/Verde.jpg");
            // y así para cada posible valor en el tablero
            default:
                return null; // o una imagen por defecto en caso de que el valor no tenga una imagen asignada
        }
    }

}
