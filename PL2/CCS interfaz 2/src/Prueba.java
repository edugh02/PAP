import scala.Tuple2;
import scala.collection.immutable.List;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;

public class Prueba extends JFrame{

    //Inicializacion del JPanel


    public JPanel panelSup = new JPanel() {
        private Image fondo = ImageIO.read(new File("src/imagenes/pared-rota2.png"));
        {
            //establecer el layout
            setLayout(new GridBagLayout());
        }

        @Override
        public void paintComponent(Graphics g) {
            //pintar el fondo
            super.paintComponent(g);
            g.drawImage(fondo, 0, 0, this.getWidth(), this.getHeight(), this);
        }
    };
    public JPanel panelInf;
    private JButton Siguiente;
    private JTable matrix;
    private JLabel filaO;
    private JLabel columnaO;
    private JLabel codx;
    private JLabel cody;
    private JLabel vidasRest;
    private JLabel nVidas;
    private int filas;
    private int columnas;
    private int limiteSuperior;
    private int limiteInferior=1;
    private int modo;
    private int dificultad;
    private int vida;
    private int x;
    private int y;
    public List<Object> matriz;

    public List<Object> aux;
    private Main clasePrincipal;
    private Tuple2<Object, Object> coordenadas;

    private int cordx=0, cordy=0;

    private int vidRest=5;

    public Prueba(List<Object> matriz, int filas, int columnas, int limiteSuperior, int limiteInferior, int modo, int dificultad, int vidas, int x, int y) throws IOException {
        //Declaracion de variables
        this.x = x;
        this.y = y;
        this.vida = vidas;
        this.modo = modo;
        this.filas = filas;
        this.matriz = matriz;
        this.columnas = columnas;
        this.dificultad = dificultad;
        this.limiteSuperior = limiteSuperior;
        this.limiteInferior = limiteInferior;

        //Ajustes de ventana
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                //hacer ventana de tamaño de la pantalla
                setExtendedState(JFrame.MAXIMIZED_BOTH);
            }
        });
        setVisible(true);

        //Matriz auxiliar que se ira modificando
        aux = matriz;

        //Inicializacion del JTable
        DefaultTableModel model = setModelo();
        matrix.setModel(model);



        //Rellenar de color la JTable
        actualizarTablero(aux, 0, 0);

       //Inicializacion de JLabels visibles dependiendo del modo de juego
        if (modo==1){
            //Obtencion de las mejores coordenadas
            coordenadas = clasePrincipal.mejores_coordenadas(aux, filas, columnas, 0, 0, 0, 0, 0);
            cordx = (int) coordenadas._1();
            cordy = (int) coordenadas._2();
            codx.setText(String.valueOf(cordx));
            cody.setText(String.valueOf(cordy));
        }else{
            codx.setVisible(false);
            cody.setVisible(false);
            filaO.setVisible(false);
            columnaO.setVisible(false);
            Siguiente.setVisible(false);
        }


        //Inicializacion de las coordenadas de donde iran los elementos visibles

        panelInf= new JPanel();
        panelInf.setBackground(Color.BLACK);
        panelInf.setSize(500, 500);

        GridBagConstraints c = new GridBagConstraints();

        //Agregar matrix
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 20, 20);
        panelSup.add(matrix, c);

        //Agregar vidasRest
        vidasRest.setBounds(20, 50, 20, 20);
        panelInf.add(vidasRest);

        //Agregar nVidas
        nVidas.setBounds(20, 50, 20, 20);
        nVidas.setText(String.valueOf(vidRest));
        panelInf.add(nVidas);

        //Agregar filaOIZONTAL;
        filaO.setBounds(20, 50, 20, 20);
        panelInf.add(filaO);

        //Agregar cody
        codx.setBounds(20, 50, 20, 20);
        panelInf.add(codx);

        //Agregar columnaO
        columnaO.setBounds(20, 50, 20, 20);
        panelInf.add(columnaO);

        //Agregar cody
        cody.setBounds(20, 50, 20, 20);
        panelInf.add(cody);


        //Agregar Siguiente
        c.gridx = 0;
        if (columnas<4) c.gridy = 4;
        else c.gridy = 3;
        c.gridwidth = 4;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(20, 20, 20, 20);
        panelInf.add(Siguiente, c);

        // Agregar paneles a la ventana jframe
        add(panelSup, BorderLayout.CENTER);
        add(panelInf, BorderLayout.SOUTH);

        Siguiente.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //Elemento seleccionado
                int elemento = clasePrincipal.getElem(aux, cordx*columnas+cordy);

                //Matriz auxiliar con todos los caramelos posibles borrados
                List<Object> auxiliar=clasePrincipal.ver_candy(aux, filas, columnas, cordx, cordy, elemento);
                int contador = clasePrincipal.contador_borrados(auxiliar, filas, columnas, 0);

                //Si solo se ha eliminado un elemento normal se resta una vida
                if (contador==1 && elemento<7){
                    vidRest--;
                    nVidas.setText(String.valueOf(vidRest));
                }

                //Actualizacion del JTable
                aux=clasePrincipal.actualizarMatriz(aux,vidas,filas,columnas,cordx,cordy,modo,dificultad,limiteInferior,limiteSuperior);
                matrix.setModel(setModelo());
                actualizarTablero(aux, 0, 0);

                if (vidRest==0){
                    //Fin de juego
                    Siguiente.setVisible(false);
                    codx.setVisible(false);
                    cody.setVisible(false);
                    filaO.setVisible(false);
                    columnaO.setVisible(false);
                }else{
                    //Muestra las mejores coordenadas para el siguiente movimiento
                    coordenadas = clasePrincipal.mejores_coordenadas(aux, filas, columnas, 0, 0, 0, 0, 0);
                    cordx= (int) coordenadas._1();
                    cordy= (int) coordenadas._2();
                    codx.setText(String.valueOf(cordx));
                    cody.setText(String.valueOf(cordy));
                }
            }
        });


        matrix.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent event) {
                if (!event.getValueIsAdjusting()) {
                    //Comprobacion de que el usuario este en modo manual y tenga vidas para seguir jugando
                    if (vidRest!=0 && modo!=1){
                        //Comprobacion de que se ha seleccionado una celda
                        if (matrix.getSelectedRow()!=-1 || matrix.getSelectedColumn()!=-1){
                            //Obtencion de las coordenadas de la celda seleccionada
                            cordx=  matrix.getSelectedRow();
                            cordy = matrix.getSelectedColumn();

                            //Elemento seleccionado
                            int elemento = clasePrincipal.getElem(aux, cordx*columnas+cordy);

                            //Matriz auxiliar con todos los caramelos posibles borrados
                            List<Object> auxiliar=clasePrincipal.ver_candy(aux, filas, columnas, cordx, cordy, elemento);
                            int contador = clasePrincipal.contador_borrados(auxiliar, filas, columnas, 0);

                            //Si solo se ha eliminado un elemento normal se resta una vida
                            if (contador==1 && elemento<7){
                                vidRest--;
                                nVidas.setText(String.valueOf(vidRest));
                            }

                            //Actualizacion del JTable
                            aux=clasePrincipal.actualizarMatriz(aux,vidas,filas,columnas,cordx,cordy,modo,dificultad,limiteInferior,limiteSuperior);
                            matrix.setModel(setModelo());
                            actualizarTablero(aux, 0, 0);
                        }

                    }
                }
            }
        });
    }

    public DefaultTableModel setModelo() {
        DefaultTableModel model = new DefaultTableModel(filas, columnas) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        return model;
    }

    public void actualizarTablero(List<Object> mat, int fil, int col)
    {
        //Comprobacion para que no se salga de los limites
        if (fil < filas && col < columnas)
        {
            //Obtencion del elemento a insertar
            int elemento = (int) clasePrincipal.getElem(mat,fil*columnas+col);

            //Comprobacion de que tipo es el elemento
            if (elemento<7){
                matrix.setValueAt(elemento, fil, col);
            }else if (elemento==7){
                matrix.setValueAt("Bomba", fil, col);
            }else if (elemento==8){
                matrix.setValueAt("TNT", fil, col);
            }else if (elemento==9){
                matrix.setValueAt("R1", fil, col);
            }else if (elemento==10){
                matrix.setValueAt("R2", fil, col);
            }else if (elemento==11){
                matrix.setValueAt("R3", fil, col);
            }else if (elemento==12){
                matrix.setValueAt("R4", fil, col);
            }else if (elemento==13){
                matrix.setValueAt("R5", fil, col);
            }else if (elemento==14){
                matrix.setValueAt("R6", fil, col);
            }

            // Establecer el color de fondo según el valor del elemeto en la celda
            matrix.setRowHeight(fil, 30); // Cambia la altura de la fila
            matrix.getColumnModel().getColumn(col).setCellRenderer(new DefaultTableCellRenderer() {
                @Override
                public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
                {
                    Component componente = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
                    int elemento =  clasePrincipal.getElem(mat, row * columnas + column);
                    Color colorFondo = Color.RED;
                    Color colorLetra = Color.WHITE;
                    switch (elemento)
                    {
                        case 1:
                            colorFondo = new Color(0, 200, 255); // azul clarito
                            colorLetra = colorFondo;
                            break;
                        case 2:
                            colorFondo = new Color(238, 62, 62, 255);
                            colorLetra = colorFondo;
                            break;
                        case 3:
                            colorFondo = new Color(196, 15, 246, 255); // lila
                            colorLetra = colorFondo;
                            break;
                        case 4:
                            colorFondo = Color.GREEN;
                            colorLetra = colorFondo;
                            break;
                        case 5:
                            colorFondo = new Color(0, 0, 229); // azul oscuro
                            colorLetra = colorFondo;
                            break;
                        case 6:
                            colorFondo = Color.YELLOW;
                            colorLetra = colorFondo;
                            break;
                        case 7, 8:
                            colorFondo = Color.BLACK;
                            colorLetra = Color.WHITE;
                            break;
                        case 9:
                            colorFondo = Color.BLACK;
                            colorLetra = new Color(0, 200, 255); // azul clarito;
                            break;
                        case 10:
                            colorFondo = Color.BLACK;
                            colorLetra = Color.RED;
                            break;
                        case 11:
                            colorFondo = Color.BLACK;
                            colorLetra = new Color(180, 140, 200); // lila
                            break;
                        case 12:
                            colorFondo = Color.BLACK;
                            colorLetra = Color.GREEN;
                            break;
                        case 13:
                            colorFondo = Color.BLACK;
                            colorLetra = new Color(0, 0, 150); // azul oscuro
                            break;
                        case 14:
                            colorFondo = Color.BLACK;
                            colorLetra = Color.YELLOW;
                            break;
                        default:
                            colorLetra = Color.WHITE;
                            break;
                    }
                    setHorizontalAlignment(SwingConstants.CENTER);
                    componente.setBackground(colorFondo);
                    componente.setForeground(colorLetra);
                    return componente;
                }
            });

            // Llamar a la función recursivamente para la siguiente celda
            if (col == columnas - 1) {
                actualizarTablero(mat, fil + 1, 0);
            } else {
                actualizarTablero(mat, fil, col + 1);
            }
        }
    }
}
