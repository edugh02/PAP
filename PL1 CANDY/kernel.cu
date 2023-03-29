
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
        if (fila != n-1) {
            ver_candy(mat, n, m, colum, fila + 1, vector,elemento);
        }
        if (colum != 0) {
            ver_candy(mat, n, m, colum - 1, fila, vector,elemento);
        }
        if (colum != m-1) {
            ver_candy(mat, n, m, colum + 1, fila, vector,elemento);
        }
        
    }
    
    
}

int cuantas_posiciones(int* vector, int n, int m) {
    int contador = 0;
    for (int i = 0; i < n * m; ++i) {
        if (vector[i] != -1) {
            ++contador;
        }
    }
    return contador;
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

__global__ void rellenar_huecos(int* mat, int n, int m, int lim_inf, int lim_sup, unsigned int ale, curandState* state) {
        // Calcular las coordenadas x e y del hilo
        int idx = threadIdx.x + blockDim.x * blockIdx.x;
        int idy = threadIdx.y + blockDim.y * blockIdx.y;

        // Verificar si el hilo se encuentra dentro de los límites de la matriz
        if (idx < m && idy < n && mat[idy * m + idx] == -1) {
            // Inicializar el generador de números aleatorios
            curand_init(ale, idy * m + idx, 0, &state[idy * m + idx]);
            // Generar un número aleatorio entero entre "lim_inf" y "lim_sup"
            int val = curand(&state[idy * m + idx]) % lim_sup + lim_inf;
            
            // Asignar el valor aleatorio al hueco
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
__global__ void eliminar5(int* mat, int n, int m, int* vector,int fila,int columna) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    bool centinela = false;
    int pos=fila*m+columna;

    if (vector[0] != -1) {
        mat[fila * m + columna] = 7;
        vector[pos] = -1;
    }

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

void eliminar5_host(int* mat, int n, int m, int* vector, int fila, int columna) {

    int* d_mat;
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));
    //copiamos la matriz del host al kernel
    cudaMemcpy(d_mat, mat, n * m * sizeof(int), cudaMemcpyHostToDevice);

    // Definimos las dimensiones de los bloques y de la grid
    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    // Llamamos al kernel "eliminar7oMas"
    eliminar5<< <gridSize, blockSize >> > (d_mat, n, m, vector, fila, columna);

    //copiamos de vuelva la matriz del kernel al host
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
}

__global__ void eliminar6(int* mat, int n, int m, int* vector,int fila,int columna) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    bool centinela = false;
    int pos = fila * m + columna;


    if (vector[0] != -1) {
        mat[fila * m + columna] = 8;
        vector[pos] = -1;
    }

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


void eliminar6_host(int* mat, int n, int m, int* vector, int fila, int columna) {

    int* d_mat;
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));
    //copiamos la matriz del host al kernel
    cudaMemcpy(d_mat, mat, n * m * sizeof(int), cudaMemcpyHostToDevice);

    // Definimos las dimensiones de los bloques y de la grid
    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    // Llamamos al kernel "eliminar7oMas"
    eliminar6 << <gridSize, blockSize >> > (d_mat, n, m, vector, fila, columna);

    //copiamos de vuelva la matriz del kernel al host
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
}

__global__ void eliminar7oMas(int* mat, int n, int m, int* vector,int fila,int columna, unsigned int ale, curandState* state) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    bool centinela = false;
    int pos = fila * m + columna;

    curand_init(ale, idy * m + idx, 0, &state[idy * m + idx]);
    // Generar un número aleatorio entero entre "lim_inf" y "lim_sup"
    int val_ale = curand(&state[idy * m + idx]) % 1 + 0;


    //generar numero aleatorio que indicará que tipo de r se forma

    if (vector[0] != -1) {  //9-->R1 10-->R2 11-->R3 21-->R4 13-->R5 14-->R6
        if (val_ale == 1) {
            mat[fila * m + columna] = 9;
        }
        else if (val_ale == 2) {
            mat[fila * m + columna] = 10;
        }
        else if (val_ale == 3) {
            mat[fila * m + columna] = 11;
        }
        else if (val_ale == 4) {
            mat[fila * m + columna] = 12;
        }
        else if (val_ale == 5) {
            mat[fila * m + columna] = 13;
        }
        else {
            mat[fila * m + columna] = 14;
        }
        vector[pos] = -1;
    }

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


void eliminar7oMas_host(int* mat, int n, int m, int* vector, int fila, int columna, unsigned int ale, curandState* state) {
    
    int* d_mat;
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));
    //copiamos la matriz del host al kernel
    cudaMemcpy(d_mat, mat, n * m * sizeof(int), cudaMemcpyHostToDevice);

    //generamos la semilla para luego crear un número aleatorio
    curandState* d_state;
    cudaMalloc((void**)&d_state, n * m * sizeof(curandState));
    unsigned int ale = generate_seed();

    // Definir las dimensiones de los bloques y de la grid
    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    // Llamar al kernel "eliminar7oMas"
    eliminar7oMas << <gridSize, blockSize >> > (d_mat, n, m, vector, fila, columna, ale, state);

    //copiamos de vuelva la matriz del kernel al host
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
    cudaFree(d_state);

 }




__global__  void caer_caramelos(int* matriz, int n, int m) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;


    // Contar los elementos -1 debajo del hilo
    int num_minus_1 = 0;
    for (int i = idy; i < n; ++i) {
        if (matriz[i * m + idx] == -1) {
            num_minus_1++;
        }
    }
    __syncthreads();
    // Buscar el primer elemento -1 debajo del hilo y intercambiarlo
    if (num_minus_1 > 0 && matriz[idy * m + idx] != -1) {
        int aux = matriz[idy * m + idx];
        matriz[idy * m + idx] = -1;
        matriz[(idy + num_minus_1) * m + idx] = aux;
    }
}



void caer_caramelos_host(int* matriz, int n, int m) {
    int size = n * m * sizeof(int);
    int* d_matriz;

    cudaMalloc((void**)&d_matriz, size);
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
    cudaMalloc((void**)&d_v, n * m * sizeof(int));

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
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));

    curandState* d_state;
    cudaMalloc((void**)&d_state, n * m * sizeof(curandState));

    unsigned int ale = generate_seed();

    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    random_matrix << <num_blocks, block_size >> > (d_mat, n, m, lim_inf, lim_sup, ale, d_state);
    
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
    cudaFree(d_state);
}

void rellenar_huecos_host(int* mat, int n, int m, int lim_inf, int lim_sup) {
    int* d_mat;
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));
    cudaMemcpy(d_mat, mat, n * m * sizeof(int), cudaMemcpyHostToDevice);


    curandState* d_state;
    cudaMalloc((void**)&d_state, n * m * sizeof(curandState));

    unsigned int ale = generate_seed();

    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    rellenar_huecos << <num_blocks, block_size >> > (d_mat, n, m, lim_inf, lim_sup, ale, d_state);

    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
    cudaFree(d_state);
}


__global__ void explotarBomba(int* mat, int n, int m, int fila, int columna, unsigned int ale, curandState* state) {
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;

    curand_init(ale, idy * m + idx, 0, &state[idy * m + idx]);
    // Generar un número aleatorio entero entre "lim_inf" y "lim_sup"
    int tipo = curand(&state[idy * m + idx]) % 1 + 0;

    if (tipo == 0) {//Eliminar la fila entera
        for (int i = 0; i < idx; ++i) {
            mat[fila * m + i] = -1;
        }
    }
    else {//Eliminar la columna entera
        for (int i = 0; i < idy; ++i) {
            mat[i * m + columna] = -1;
        }
    }
}



void explotarBomba_host(int* mat, int n, int m, int* vector, int fila, int columna) {

    int* d_mat;
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));
    //copiamos la matriz del host al kernel
    cudaMemcpy(d_mat, mat, n * m * sizeof(int), cudaMemcpyHostToDevice);

    //generamos la semilla para luego crear un número aleatorio
    curandState* d_state;
    cudaMalloc((void**)&d_state, n * m * sizeof(curandState));
    unsigned int ale = generate_seed();

    // Definimos las dimensiones de los bloques y de la grid
    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    // Llamamos al kernel "eliminar7oMas"
    explotarBomba << <gridSize, blockSize >> > (d_mat, n, m, vector, fila, columna, ale, state);

    //copiamos de vuelva la matriz del kernel al host
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
}

__global__ void explotarTNT(int* mat, int n, int m, int fila, int columna) {                                          // ESTE HAY Q CAMBIARLO Q NO LO HE QUERIDO TOCAR
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    mat[fila * m + columna] = -1;
    if (fila != 0) {
        if (columna != 0) {
            mat[(fila - 1) * m + columna - 1] = -1;
        }
        else if (columna != m - 1) {
            mat[(fila - 1) * m + columna + 1] = -1; 
        }
        if (0 < mat[(fila - 1) * m + columna] && mat[(fila - 1) * m + columna] < 7) {
            mat[(fila - 1) * m + columna] = -1;
        }
        else if (mat[(fila - 1) * m + columna] == 7) {
            explotarBomba(mat, n, m, fila - 1, columna);
        }
        else if (mat[(fila - 1) * m + columna] == 8) {
            explotarTNT(mat, n, m, fila - 1, columna);
        }
        else if (mat[(fila - 1) * m + columna] > 8) {
            explotarRx(mat, n, m, fila - 1, columna, mat[(fila - 1) * m + columna]);
        }
    }
    else if (fila != n - 1) {
        if (columna != 0) {
            mat[(fila + 1) * m + columna - 1] = -1;
        }
        else if (columna != m - 1) {
            mat[(fila + 1) * m + columna + 1] = -1;
        }
        mat[(fila + 1) * m + columna] = -1;
    }
    if (columna != 0) {
        mat[fila * m + columna - 1] = -1;
    }
    else if (columna != m - 1) {
        mat[fila * m + columna + 1] = -1;
    }
}


void explotarTNT_host(int* mat, int n, int m, int* vector, int fila, int columna) {

    int* d_mat;
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));
    //copiamos la matriz del host al kernel
    cudaMemcpy(d_mat, mat, n * m * sizeof(int), cudaMemcpyHostToDevice);

    // Definimos las dimensiones de los bloques y de la grid
    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    // Llamamos al kernel "eliminar7oMas"
    explotarTNT << <gridSize, blockSize >> > (d_mat, n, m, vector, fila, columna);

    //copiamos de vuelva la matriz del kernel al host
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
}

__global__ void explotarRx(int* matriz, int n, int m, int fila, int columna, int tipo) {                   //HACER HOST PARA ESTA, NO SABIA COMO SACAR EL TIPO
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    int pos = idy * m + idy;
    switch (tipo) {
    case 9:
        if (matriz[pos] == 1) {
            matriz[pos = -1];
        }
        break;
    case 10:
        if (matriz[pos] == 2) {
            matriz[pos = -1];
        }
        break;
    case 11:
        if (matriz[pos] == 3) {
            matriz[pos = -1];
        }
        break;
    case 12:
        if (matriz[pos] == 4) {
            matriz[pos = -1];
        }
        break;
    case 13:
        if (matriz[pos] == 5) {
            matriz[pos = -1];
        }
        break;
    case 14:
        if (matriz[pos] == 6) {
            matriz[pos = -1];
        }
        break;
    default:
        break;
    }
}



void eliminar_elementos(int* matriz, int n, int m, int* vector, int fila, int columna) {                       //AQUI HAY Q LLAMAR BIEN A LOS MÉTODOS DEL KERNEL
    int* d_matriz;
    int* d_vector;
    int tamVector = n * m;

    // Alocar memoria para la matriz y el vector en la GPU
    cudaMalloc((void**)&d_matriz, n * m * sizeof(int));
    cudaMalloc((void**)&d_vector, tamVector * sizeof(int));

    // Copiar la matriz y el vector de la CPU a la GPU
    cudaMemcpy(d_matriz, matriz, n * m * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(d_vector, vector, tamVector * sizeof(int), cudaMemcpyHostToDevice);

    // Definir la configuración del kernel
    dim3 blockSize(16, 16);
    dim3 gridSize((n + blockSize.x - 1) / blockSize.x, (m + blockSize.y - 1) / blockSize.y);

    // Llamar al kernel
    switch (cuantas_posiciones(vector, n, m)) {
    case 1: //TENEMOS QUE ACORDARNOS DE BORRAR LA PRIMERA POSICION DEL VECTOR, QUE SE HABRÁ RELLENADO
        if (matriz[vector[0]] == 1 || matriz[vector[0]] == 2 || matriz[vector[0]] == 3 || matriz[vector[0]] == 4 || matriz[vector[0]] == 5 || matriz[vector[0]] == 6) {
            --vidas;
        }
        else if (matriz[vector[0]] == 7) {
            //BOMBA
            explotarBomba(matriz, n, m, fila, columna);
        }
        else if (matriz[vector[0]] == 8) {
            //TNT
            explotarTNT(matriz, n, m, fila, columna);
        }
        else if (matriz[vector[0]] > 8) {
            //Rx
            explotarRx(matriz, n, m, fila, columna, matriz[fila * m + columna]);
        }
        vector[0] = -1;
        break;
    case 2:
        eliminar_iguales_juntos << <gridSize, blockSize >> > (d_matriz, n, m, d_vector);
        break;
    case 3:
        eliminar_iguales_juntos << <gridSize, blockSize >> > (d_matriz, n, m, d_vector);
        break;
    case 4:
        eliminar_iguales_juntos << <gridSize, blockSize >> > (d_matriz, n, m, d_vector);
        break;
    case 5:
        //Kernel sustituir el elemento de la posición por un B y borrar el resto
        eliminar5 << <gridSize, blockSize >> > (d_matriz, n, m, d_vector, fila, columna);
        break;
    case 6:
        //Kernel sustituir el elemento de la posición por un TNT y borrar el resto
        eliminar6 << <gridSize, blockSize >> > (d_matriz, n, m, d_vector, fila, columna);
        break;
    default:
        //Kernel sustituir el elemento de la posición por un Rx y borrar el resto
        eliminar7oMas << <gridSize, blockSize >> > (d_matriz, n, m, d_vector, fila, columna);
        break;
    }


    // Copiar la matriz resultante de la GPU a la CPU
    cudaMemcpy(matriz, d_matriz, n * m * sizeof(int), cudaMemcpyDeviceToHost);
    cudaMemcpy(vector, d_vector, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    // Liberar la memoria de la GPU
    cudaFree(d_matriz);
    cudaFree(d_vector);
}


void imprimir(int* matriz, int n, int m) {
    printf("\n");

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (matriz[i * m + j]==7) {
                printf("B   ");
            }else if (matriz[i * m + j] == 8) {
                printf("T   ");
            }
            else if (matriz[i * m + j] == 9) {
                printf("R1  ");
            }
            else if (matriz[i * m + j] == 10) {
                printf("R2  ");
            }
            else if (matriz[i * m + j] == 11) {
                printf("R3  ");
            }
            else if (matriz[i * m + j] == 12) {
                printf("R4  ");
            }
            else if (matriz[i * m + j] == 13) {
                printf("R5  ");
            }
            else if (matriz[i * m + j] == 14) {
                printf("R6  ");
            }
            else {
                printf("%d   ", matriz[i * m + j]);
            }
        }
        printf("\n");
    }
}


int main()
{
    srand(time(NULL));
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
         lim_sup = 2; // valor máximo
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
            else {              
                colum =rand() %m;
                fila = rand() %n;
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

        eliminar_elementos(mat, n, m, posicionesVistas,fila,colum);
        printf("\n\n");
        imprimir(mat,n,m);
        printf("\n\n");
        caer_caramelos_host(mat, n, m);
        imprimir(mat, n, m);
        rellenar_huecos_host(mat, n, m, lim_inf, lim_sup);
    }

    free(mat);

    return 0;
}