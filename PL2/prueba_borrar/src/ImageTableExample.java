import javax.swing.*;
import javax.swing.table.TableColumn;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

class ImageTableCellRenderer extends DefaultTableCellRenderer {

    private Map<Integer, ImageIcon> imageMap;

    public ImageTableCellRenderer() {
        super();
        // inicializar el mapa de imágenes para cada número
        imageMap = new HashMap<>();
        imageMap.put(1, new ImageIcon("ruta/imagen1.png"));
        imageMap.put(2, new ImageIcon("ruta/imagen2.png"));
        // ... agregar más imágenes al mapa
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        // Obtener el valor numérico de la celda
        int number = (int) value;
        // Obtener la imagen correspondiente al número del mapa
        ImageIcon imageIcon = imageMap.get(number);
        // Crear un JLabel para mostrar la imagen
        JLabel label = new JLabel(imageIcon);
        label.setOpaque(true);
        // Configurar el color de fondo para mostrar la selección de la celda
        if (isSelected) {
            label.setBackground(table.getSelectionBackground());
        } else {
            label.setBackground(table.getBackground());
        }
        return label;
    }
}

    public static void main(String[] args) {
        // Obtener la columna que desea configurar para mostrar imágenes
        TableColumn imageColumn = table.getColumnModel().getColumn(columnIndex);
// Establecer la clase personalizada ImageTableCellRenderer como el renderizador de celda para esta columna
        imageColumn.setCellRenderer(new ImageTableCellRenderer());

    }
}
