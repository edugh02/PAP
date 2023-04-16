import scala.Tuple2;
import scala.collection.immutable.List;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Interfaz {
    private JSpinner filasSpinner;
    private JSpinner columnasSpinner;
    private JComboBox modoComboBox;
    private JComboBox dificultadComboBox;
    private JButton jugarButton;
    private JPanel Fondo;
    private JButton escogerButton;
    private JSpinner filaObjetivo1;
    private JSpinner columnaObjetivo2;
    private JLabel fila;      /// borrar si no la usamos
    private int modo;
    private int dificultad;

    private int filas;
    private int columnas;
    private int limiteSuperior;
    private int limiteInferior=1;
    private Main clasePrincipal;
    private List<Object> matriz;
    private int x;
    private int y;
    private int vidas=5;
    private Tuple2<Object, Object> coordenadas;
    private JFrame tablero = new JFrame("Tablero");

    public Interfaz() {
        jugarButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                modo = modoComboBox.getSelectedIndex()+1;
                dificultad = dificultadComboBox.getSelectedIndex()+1;
                filas = (int)filasSpinner.getValue();
                columnas = (int)columnasSpinner.getValue();
                if (dificultad == 1){
                    limiteSuperior = 4;
                }else limiteSuperior = 6;

                matriz = clasePrincipal.crearMatrizAleatoria(0,filas,columnas,limiteInferior,limiteSuperior);
                clasePrincipal.jugar(matriz,vidas,modo,dificultad,filas,columnas,limiteInferior,limiteSuperior);
                tablero.setContentPane(new Tablero(matriz,filas,columnas,limiteSuperior,limiteInferior,modo,dificultad,vidas,x,y).tablero);
                tablero.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                tablero.pack();
                tablero.setVisible(true);
                jugarButton.setVisible(false);
            }
        });
        escogerButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (modo==1){
                    coordenadas = clasePrincipal.mejores_coordenadas(matriz,filas,columnas,0,0,0,0,0);
                    x = (int)coordenadas._1();
                    y = (int)coordenadas._2();
                    matriz=clasePrincipal.actualizarMatriz(matriz,vidas,filas,columnas,x,y,modo,dificultad,limiteInferior,limiteSuperior);
                    tablero.setContentPane(new Tablero(matriz,filas,columnas,limiteSuperior,limiteInferior,modo,dificultad,vidas,x,y).tablero);
                    tablero.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                    tablero.pack();
                    tablero.setVisible(true);
                }else {
                    x = (int)filaObjetivo1.getValue();
                    y = (int)columnaObjetivo2.getValue();
                    matriz=clasePrincipal.actualizarMatriz(matriz,vidas,filas,columnas,x,y,modo,dificultad,limiteInferior,limiteSuperior);
                    tablero.setContentPane(new Tablero(matriz,filas,columnas,limiteSuperior,limiteInferior,modo,dificultad,vidas,x,y).tablero);
                    tablero.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                    tablero.pack();
                    tablero.setVisible(true);
                }
                clasePrincipal.jugar(matriz,vidas,modo,dificultad,filas,columnas,limiteInferior,limiteSuperior);
            }
        });
    }

    public void establecerImagen(){

    }


    public static void main(String[] args) {

        JFrame frame = new JFrame("Interfaz");
        frame.setContentPane(new Interfaz().Fondo);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);

    }

}
