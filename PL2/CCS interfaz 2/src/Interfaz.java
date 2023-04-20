import scala.collection.immutable.List;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Graphics;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import javax.swing.JPanel;

public class Interfaz extends JFrame{
    private JSpinner filasSpinner;
    private JSpinner columnasSpinner;

    private JLabel Titulo;
    private JLabel NumeroFJL;
    private JLabel NumeroCJL;
    private JLabel modoJL;
    private JLabel dificultadJL;
    private JComboBox modoComboBox;
    private JComboBox dificultadComboBox;
    private JButton jugarButton;
    private JPanel Fondo;
    private int modo;
    private int dificultad;
    private int filas;
    private int columnas;
    private int limiteSuperior;
    private int limiteInferior=1;
    private Main clasePrincipal;
    private List<Object> matriz;
    private int vidas;

    public Interfaz() throws IOException {


        Fondo = new JPanel() {
            private Image fondo = ImageIO.read(new File("src/imagenes/pared4.jpg"));
            {
                setLayout(new GridBagLayout());
            }

            @Override
            public void paintComponent(Graphics g) {
                super.paintComponent(g);
                g.drawImage(fondo, 0, 0, getWidth(), getHeight(), this);
            }
        };


        //Ajustes de componentes
        GridBagConstraints c = new GridBagConstraints();

        c.gridx = 0;
        c.gridy = 0;
        c.fill = GridBagConstraints.CENTER; // Ocupa todas las celdas en ambas direcciones
        c.insets = new Insets(20, 20, 20, 20);
        c.anchor = GridBagConstraints.CENTER; // Centra el componente en la celda
        Fondo.add(Titulo);


        //Agregar dificultadJL
        c.gridx = 0;
        c.gridy = 1;
        c.fill = GridBagConstraints.CENTER; // Ocupa todas las celdas en ambas direcciones
        c.insets = new Insets(20, 20, 20, 20);
        c.anchor = GridBagConstraints.CENTER; // Centra el componente en la celda
        Fondo.add(dificultadJL, c);

        //Agregar dificultadComboBox
        c.gridx = 1;
        c.gridy = 1;
        c.fill = GridBagConstraints.CENTER;
        c.insets = new Insets(20, 20, 20, 20);
        Fondo.add(dificultadComboBox, c);

        //Agregar modoJL
        c.gridx = 0;
        c.gridy = 2;
        c.fill = GridBagConstraints.CENTER;
        c.insets = new Insets(20, 20, 20, 20);
        Fondo.add(modoJL, c);

        //Agregar modoComboBox
        c.gridx = 1;
        c.gridy = 2;
        c.fill = GridBagConstraints.CENTER;
        c.insets = new Insets(20, 20, 20, 20);
        Fondo.add(modoComboBox, c);

        //Agregar NumeroFJL
        c.gridx = 0;
        c.gridy = 3;
        c.fill = GridBagConstraints.CENTER;
        c.insets = new Insets(20, 20, 20, 20);
        Fondo.add(NumeroFJL, c);

        SpinnerModel filasModelo = new SpinnerNumberModel(1, 1, 200, 2);
        filasSpinner = new JSpinner(filasModelo);

        // Establecer las dimensiones preferidas del spinner
        filasSpinner.setPreferredSize(new Dimension(50, 20));

        // Agregar el spinner al panel
        c.gridx = 1;
        c.gridy = 3;
        c.fill = GridBagConstraints.CENTER;
        c.insets = new Insets(20, 20, 20, 20);
        Fondo.add(filasSpinner, c);

        //Agregar NumeroCJL
        c.gridx = 0;
        c.gridy = 4;
        c.fill = GridBagConstraints.CENTER;
        c.insets = new Insets(20, 20, 20, 20);
        Fondo.add(NumeroCJL, c);

        //Crear columnasSpinner
        SpinnerModel columnasModelo = new SpinnerNumberModel(1, 1, 200, 2);
        JSpinner columnasSpinner = new JSpinner(columnasModelo);
        columnasSpinner.setPreferredSize(new Dimension(50, 20)); //Establecer tamaño preferido

        //Agregar columnasSpinner
        c.gridx = 1;
        c.gridy = 4;
        c.fill = GridBagConstraints.CENTER;
        c.insets = new Insets(20, 20, 20, 20);
        Fondo.add(columnasSpinner, c);

        //Agregar jugarButton
        c.gridx = 0;
        c.gridy = 5;
        c.gridwidth = GridBagConstraints.REMAINDER;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 20, 20);
        Fondo.add(jugarButton, c);

        jugarButton.addActionListener(new ActionListener()
        {
            @Override
            public void actionPerformed(ActionEvent e)
            {
                //Obtencion de datos desde la interfaz
                filas = (int)filasSpinner.getValue();
                modo = modoComboBox.getSelectedIndex()+1;
                columnas = (int)columnasSpinner.getValue();
                dificultad = dificultadComboBox.getSelectedIndex()+1;
                if (dificultad == 1)
                {
                    limiteSuperior = 4;
                }
                else limiteSuperior = 6;

                //Creacion de la matriz
                matriz = clasePrincipal.crearMatrizAleatoria(0,filas,columnas,limiteInferior,limiteSuperior);

                //Llamada la nueva ventana

                try {
                    new Prueba(matriz,filas,columnas,limiteSuperior,limiteInferior,modo,dificultad, vidas,0,0);
                } catch (IOException ex) {
                    throw new RuntimeException(ex);
                }

                //Cerrar la ventana actual
                JFrame ventanaActual = (JFrame) SwingUtilities.getWindowAncestor((Component) e.getSource());
                ventanaActual.dispose();
            }
        });

    }

    public static void main(String[] args) throws IOException {
        JFrame frame = new JFrame("Cundy crosh soga");
        frame.setContentPane(new Interfaz().Fondo);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        frame.setVisible(true);
        frame.setLocationRelativeTo(null);
    }

}
