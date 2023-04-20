import scala.collection.immutable.List;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;

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
        tablero.setDefaultRenderer(Object.class, new ImgTabla());
        tablero.setRowHeight(50);
        actualizarTablero(matriz,0,0);
    }

    public void windowOpened(WindowEvent e) {
        // aquí va la función que se ejecutará cuando se abra la ventana
        tablero.setValueAt("Prueba",0,0);
        Prueba.setText("Prueba");
        tablero.setVisible(true);
    }

    public void actualizarTablero(List<Object> mat, int fil, int col) {
        if (fil < filas) {
            if (col == columnas) {
                fil++;
                col = 0;
                actualizarTablero(mat, fil, col);
            } else {
                ImageIcon imagen = obtenerImagenDesdeIndice(matriz, fil, col);
                ImageLabel label = new ImageLabel(imagen);
                label.setSize(50, 50);
                label.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseClicked(MouseEvent e) {
                        // Aquí es donde colocas la acción que deseas que ocurra al hacer clic en el JLabel
                        tablero.setVisible(false);
                    }
                });

                tablero.setValueAt(label, fil, col);
                actualizarTablero((List<Object>) mat.tail(), fil, col+1);
            }
        }
    }



    private ImageIcon obtenerImagenDesdeIndice(List<Object> mat, int fil, int col) {
        // Lógica para obtener la imagen correspondiente al índice
        // por ejemplo, con un switch:
        int indice = fil * columnas + col;
        Object elem = clasePrincipal.getElem(mat, indice);
        String ruta = "";
        switch (elem.toString()) {
            case "1":
                ruta = "src/imagenes/Azul.jpg";
                break;
            case "2":
                ruta = "src/imagenes/Verde.jpg";
                break;
            case "3":
                ruta = "src/imagenes/Rojo.jpg";
                break;
            case "4":
                ruta = "src/imagenes/Amarillo.jpg";
                break;
            case "5":
                ruta = "src/imagenes/Morado.jpg";
                break;
            case "6":
                ruta = "src/imagenes/Naranja.jpg";
                break;
            case "7":
                ruta = "src/imagenes/bomba.jpg";
                break;
            case "8":
                ruta = "src/imagenes/tnt.jpg";
                break;
            case "9":
                ruta = "src/imagenes/r1.jpg";
                break;
            case "10":
                ruta = "src/imagenes/r2.jpg";
                break;
            case "11":
                ruta = "src/imagenes/r3.jpg";
                break;
            case "12":
                ruta = "src/imagenes/r4.jpg";
                break;
            case "13":
                ruta = "src/imagenes/r5.jpg";
                break;
            case "14":
                ruta = "src/imagenes/r6.jpg";
                break;
            default:
                // en caso de que el valor no tenga una imagen asignada
                return null; // o una imagen por defecto
        }
        try {
            ImageIcon icon = new ImageIcon(ImageIO.read(new File(ruta)));
            Image imagen = icon.getImage();
            Image nuevaImagen = imagen.getScaledInstance(50, 50, Image.SCALE_SMOOTH);
            ImageIcon nuevaIcon = new ImageIcon(nuevaImagen);
            return nuevaIcon;
        } catch (IOException e) {
            // en caso de que ocurra una excepción al leer la imagen
            e.printStackTrace();
            return null; // o una imagen por defecto
        }
    }


    public class ImgTabla extends DefaultTableCellRenderer {
        @Override
        public Component getTableCellRendererComponent (JTable jtable, Object o , boolean bln, boolean bln1, int i, int i1) {
            if (o instanceof JLabel) {
                JLabel Ibl = (JLabel) o;
                return Ibl;
            }
                return super.getTableCellRendererComponent (jtable, o, bln, bln1, i, i1);


        }
    }

    public class ImageLabel extends JLabel {

        public ImageLabel(ImageIcon icon) {
            super(icon);
        }
    }



}
