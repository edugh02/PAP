
#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <curand.h>
#include <stdlib.h>
#include <stdio.h>
#include <curand_kernel.h>
#include <time.h>


/*const int fila;
const int columna;
*/
int vidas = 5;
 //y filas, x columnas --> idy*columnas + idx ( idx es todos los .x)


// Esta función genera una semilla aleatoria basada en la hora actual.
unsigned int generate_seed() {
    time_t t;
    time(&t);
    return (unsigned int)t % 100000;
}

bool esta_en_vector(int* vector, int m, int n, int pos) {
    for (int i = 0; i < n * m; ++i) {
        if (vector[i] == pos) {
            return true;
        }
    }
    return false;
}

int primer_vacio(int* vector, int n, int m) {
    int x = 0;
    for (int i = 0; i < n * m; ++i) {
        if (vector[i] == -1) {
            x = i;
            i = n * m;
        }
    }
    return x;
}


void ver_candy(int* mat, int n, int m,int colum,int fila, int* vector,int elemento) {
    int caramelo = fila * m + colum;
    //printf("\nEl caramelo esta en la posicion: %d\n", caramelo);
    //printf("Está en el vector ya? %d\n",esta_en_vector(vector, m, n, caramelo));
    //printf("El elemento es igual? %d\n", mat[caramelo] == elemento);
    //printf("Elemento dentro %d \n", mat[caramelo]);

    if ( !esta_en_vector(vector,m,n,caramelo)&& mat[caramelo]==elemento) {
        int pos=primer_vacio(vector,n,m);
        printf("\nposicion del vector siguiente: %d \n", pos);
        vector[pos] = caramelo;
        if (fila != 0) {
            ver_candy(mat, n, m, colum, fila - 1, vector,elemento);
        }
        if (fila != n) {
            ver_candy(mat, n, m, colum, fila + 1, vector,elemento);
        }
        if (colum != 0) {
            ver_candy(mat, n, m, colum - 1, fila, vector,elemento);
        }
        if (colum != m) {
            ver_candy(mat, n, m, colum + 1, fila, vector,elemento);
        }
        
    }
    
    
}

// Esta función genera una matriz aleatoria de números enteros entre "lim_inf" y "lim_sup".
__global__ void random_matrix(int* mat, int n, int m, int lim_inf, int lim_sup, unsigned int ale, curandState* state) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;

    // Verificar si el hilo se encuentra dentro de los límites de la matriz
    if (idx < m && idy < n) {
        // Inicializar el generador de números aleatorios
        curand_init(ale, idy * m + idx, 0, &state[idy * m + idx]);
        // Generar un número aleatorio entero entre "lim_inf" y "lim_sup"
        int val = curand(&state[idy * m + idx]) % lim_sup + lim_inf;
        // Asignar el valor aleatorio a la matriz
        mat[idy * m + idx] = val;
    }
}



__global__ void eliminar_iguales_juntos(int* mat, int n, int m,int* vector) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    bool centinela = false;
    int pos;

    for (int i = 0; i < n * m; i++) {
        if (vector[i] == idy * m + idx) {
            centinela = true; // El número está presente en el vector
            pos = i;
        } 
    }

    // Verificar si el hilo se encuentra dentro de los límites de la matriz y coincide con una posición que hay que eliminar
    if (centinela) {
        mat[idy * m + idx] = -1;
        vector[pos] = -1;
    }
}



void eliminar_elementos(int* matriz, int n, int m, int* vector) { //NO SE SI AQUI TMBN TENEMOS QUE ELIMINAR EL ELEMENTO DEL VECTOR DE POSICIONES 
    int* d_matriz;
    int* d_vector;
    int tamVector = n*m;

    // Alocar memoria para la matriz y el vector en la GPU
    cudaMalloc(&d_matriz, n * m * sizeof(int));
    cudaMalloc(&d_vector, tamVector * sizeof(int));

    // Copiar la matriz y el vector de la CPU a la GPU
    cudaMemcpy(d_matriz, matriz, n * m * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(d_vector, vector, tamVector * sizeof(int), cudaMemcpyHostToDevice);

    // Definir la configuración del kernel
    dim3 blockSize(16, 16);
    dim3 gridSize((n + blockSize.x - 1) / blockSize.x, (m + blockSize.y - 1) / blockSize.y);

    // Llamar al kernel
    eliminar_iguales_juntos << <gridSize, blockSize >> > (d_matriz, n, m, d_vector);

    // Copiar la matriz resultante de la GPU a la CPU
    cudaMemcpy(matriz, d_matriz, n * m * sizeof(int), cudaMemcpyDeviceToHost);
    cudaMemcpy(vector, d_vector, n * m * sizeof(int), cudaMemcpyDeviceToHost);
    
    // Liberar la memoria de la GPU
    cudaFree(d_matriz);
    cudaFree(d_vector);
}


__global__ void caer_caramelos(int* matriz, int n, int m) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    int pos = idy * m + idx;
    int contador = 0;
    int aux;


    for (int i = idy; i < n; i++) {
        int elem_actual = i * m + idy;
        if (elem_actual == -1)contador++;
    }
    if (idy == 0) {
        aux=
        matriz[idy * m + idx + (idx * contador)] = 4;
    }
    
    
}



void caer_caramelos_host(int* matriz, int n, int m) {
    int size = n * m * sizeof(int);
    int* d_matriz;

    cudaMalloc(&d_matriz, size);
    cudaMemcpy(d_matriz, matriz, size, cudaMemcpyHostToDevice);

    // Configurar la cantidad de hilos por bloque y la cantidad de bloques por cuadrícula
    dim3 tamBloque(16, 16);
    dim3 tamCuadricula((n + tamBloque.x - 1) / tamBloque.x, (m + tamBloque.y - 1) / tamBloque.y);

    // Llamar al kernel caer_caramelos
    caer_caramelos << <tamCuadricula, tamBloque >> > (d_matriz, n, m);

    // Copiar la matriz resultante de la GPU al host
    cudaMemcpy(matriz, d_matriz, size, cudaMemcpyDeviceToHost);

    // Liberar memoria de la GPU
    cudaFree(d_matriz);
}


__global__ void fill(int* vec, int n) {
    int i = threadIdx.x + blockIdx.x * blockDim.x;
    if (i < n) {
        vec[i] = -1;
    }
}

void crear_vector(int* posicionesVistas, int n, int m) {
    int* d_v;
    cudaMalloc(&d_v, n * m * sizeof(int));

    // Definir la configuración del kernel
    int threadsPerBlock = 256;
    int blocksPerGrid = (n * m + threadsPerBlock - 1) / threadsPerBlock;

    // Llamar al kernel
    fill << <blocksPerGrid, threadsPerBlock >> > (d_v, n * m);

    cudaMemcpy(posicionesVistas, d_v, n * m * sizeof(int), cudaMemcpyDeviceToHost);
    cudaFree(d_v);
}



void create_random_matrix(int* mat, int n, int m, int lim_inf, int lim_sup) {
    int* d_mat;
    cudaMalloc(&d_mat, n * m * sizeof(int));

    curandState* d_state;
    cudaMalloc(&d_state, n * m * sizeof(curandState));

    unsigned int ale = generate_seed();

    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    random_matrix << <num_blocks, block_size >> > (d_mat, n, m, lim_inf, lim_sup, ale, d_state);
    
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
    cudaFree(d_state);
}



void imprimir(int* matriz, int n, int m) {
    printf("\n");

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            printf("%d  ", matriz[i * m + j]);
        }
        printf("\n");
    }
}


int generarNumAleatorio(int hasta) {
    srand(generate_seed());
    int ale = rand() % (hasta + 1);
    return ale;
}


int main()
{

    int modo; //automático o manual
    int dificultad; //dificultad del juego
    int n; // número de filas
    int m; // número de columnas
    printf("Bienvenido a Cundio Crack\n");
    printf("Introduce el modo de juego con el que quieres jugar: \n 1. Automatico \n 2. Manual \n");
    scanf("%d", &modo);
    printf("Introduce la dificultad con la que quieres jugar: \n 1. Facil \n 2. Normal \n");
    scanf("%d", &dificultad);
    printf("Introduce el numero de filas que quieres que tenga el tablero: \n");
    scanf("%d", &n);
    printf("Introduce el numero de columnas que quieres que tenga el tablero: \n");
    scanf("%d", &m);

    int lim_inf = 1; // valor mínimo
    int lim_sup = 6; // valor máximo
    if (dificultad == 1) {
         lim_sup = 4; // valor máximo
    }
    
    int* mat = (int*)malloc(n * m * sizeof(int)); // matriz aleatoria
    int* posicionesVistas = (int*)malloc(n * m * sizeof(int)); 
    crear_vector(posicionesVistas, n, m);
    create_random_matrix(mat, n, m, lim_inf, lim_sup);


    //AQUI                                                                                  //<--ESTO HAY QUE VER COMO HACERLO EFICIENTE
    dim3 dimBlock(n,m);
    dim3 dimGrid(1);

    int colum=-1;
    int fila=-1;

    while (vidas > 0) {
        imprimir(mat, n, m);
        do {
            if (modo == 2) {
                printf("Introduce la fila del caramelo que quieres comprobar\n");
                scanf("%d", &fila);
                printf("Introduce la columna del caramelo que quieres comprobar\n");
                scanf("%d", &colum);

            }
            else {                                                              //ESTA MIERDA ESTA MAL, REVISAR A VER COMO SERÍA
                srand(time(NULL));
                colum = generarNumAleatorio(n);
                srand(time(NULL));
                fila = generarNumAleatorio(m);
                printf("%d\n", colum);
                printf("%d\n", fila);
            }
        } while (colum > m && fila > n && colum < 0 && fila < 0);
        


        int elemento = mat[fila * m + colum];
        printf("Elemento antes %d \n", elemento);
        ver_candy(mat, n, m, colum, fila, posicionesVistas,elemento);
        for (int i = 0; i < n * m; ++i) {
            printf("%d ", posicionesVistas[i]);
        }
        printf("\n");

        eliminar_elementos(mat, n, m, posicionesVistas);
        printf("\n\n");
        imprimir(mat,n,m);
        printf("\n\n");
        caer_caramelos_host(mat, n, m);

        vidas -= 1;
    }

   

    free(mat);

    return 0;
}