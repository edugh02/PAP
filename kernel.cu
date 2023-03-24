
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
 //y filas, x columnas


// Esta función genera una semilla aleatoria basada en la hora actual.
unsigned int generate_seed() {
    time_t t;
    time(&t);
    return (unsigned int)t % 100000;
}

bool esta_o_no(int* vector, int m, int n, int pos) {
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


void ver_candy(int* mat, int n, int m,int cordx,int cordy, int* vector,int elemento) {
    int caramelo = cordy * n + cordx;
    printf("\nEl caramelo esta en la posicion: %d\n", caramelo);
    if ( !esta_o_no(vector,m,n,caramelo)&& mat[caramelo]==elemento) {
        int pos=primer_vacio(vector,n,m);
        printf("\nposicion del vector siguiente: %d \n", pos);
        vector[pos] = caramelo;
        if (cordy != 0) {
            ver_candy(mat, n, m, cordx, cordy - 1, vector,elemento);
        }
        if (cordy != n) {
            ver_candy(mat, n, m, cordx, cordy + 1, vector,elemento);
        }
        if (cordx != 0) {
            ver_candy(mat, n, m, cordx - 1, cordy, vector,elemento);
        }
        if (cordx != m) {
            ver_candy(mat, n, m, cordx + 1, cordy, vector,elemento);
        }
        
    }
    
    
}

// Esta función genera una matriz aleatoria de números enteros entre "lim_inf" y "lim_sup".
__global__ void random_matrix(int* mat, int n, int m, int lim_inf, int lim_sup, unsigned int ale, curandState* state) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;

    // Verificar si el hilo se encuentra dentro de los límites de la matriz
    if (idx < n && idy < m) {
        // Inicializar el generador de números aleatorios
        curand_init(ale, idx * m + idy, 0, &state[idx * m + idy]);
        // Generar un número aleatorio entero entre "lim_inf" y "lim_sup"
        int val = curand(&state[idx * m + idy]) % lim_sup + lim_inf;
        // Asignar el valor aleatorio a la matriz
        mat[idx * m + idy] = val;
    }
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
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            printf("%d ", matriz[i * m + j]);
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

    int cordx=-1;
    int cordy=-1;
    int dir=-1;

    while (vidas > 0) {
        imprimir(mat, n, m);
        do {
            if (modo == 2) {
                printf("Introduce la cordenada X del caramelo que quieres mover\n");
                scanf("%d", &cordx);
                printf("Introduce la cordenada Y del caramelo que quieres mover\n");
                scanf("%d", &cordy);

            }
            else {                                                              //ESTA MIERDA ESTA MAL, REVISAR A VER COMO SERÍA
                srand(time(NULL));
                cordx = generarNumAleatorio(n);
                srand(time(NULL));
                cordy = generarNumAleatorio(m);
                printf("%d\n", cordx);
                printf("%d\n", cordy);
            }
        } while (cordx > n && cordy > m && cordx < 0 && cordy < 0);
        


        int elemento = mat[cordx * n + cordy];
        ver_candy(mat, n, m, cordx, cordy, posicionesVistas,elemento);
        for (int i = 0; i < n * m; ++i) {
            printf("%d ", posicionesVistas[i]);
        }
        printf("\n");

        vidas -= 1;
    }

   

    free(mat);

    return 0;
}