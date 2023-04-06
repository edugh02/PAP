
#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <curand.h>
#include <stdlib.h>
#include <stdio.h>
#include <curand_kernel.h>
#include <time.h>

#define SH_DIM_Y 16
#define SH_DIM_X 16
int vidas = 5;
 //y filas, x columnas --> idy*columnas + idx ( idx es todos los .x)

//inicialización tonta para ver que luego cambia
int HILOS_BLOQUE_X=777;
int HILOS_BLOQUE_Y=777;
int BLOQUES_GRID_X=777;
int BLOQUES_GRID_Y=777;


// Esta función genera una semilla aleatoria basada en la hora actual.
unsigned int generate_seed() {
    time_t t;
    time(&t);
    return (unsigned int)t % 100000;
}

//Comprueba si una posicion esta en el vector
bool esta_en_vector(int* vector, int m, int n, int pos) {
    for (int i = 0; i < n * m; ++i) {
        if (vector[i] == pos) {
            return true;
        }
    }
    return false;
}

//retorna la primera poscion donde haya un -1
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
    int caramelo = fila * m + colum;//posicion en la matriz de las coordenadas

    if (!esta_en_vector(vector,m,n,caramelo)&& mat[caramelo]==elemento) {
        //comprobamos que la posicion no ha sido ya insertada
        int pos=primer_vacio(vector,n,m);
        vector[pos] = caramelo;
        //insertamos en la primera posicion que se encuentre vacia del vector, la posicion del caramelo
        if (fila != 0) {//Adyacente de arriba
            ver_candy(mat, n, m, colum, fila - 1, vector,elemento);
        }
        if (fila != n - 1) {//Adyacente de abajo
            ver_candy(mat, n, m, colum, fila + 1, vector,elemento);
        }
        if (colum != 0) {//Adyacente de la izquierda
            ver_candy(mat, n, m, colum - 1, fila, vector,elemento);
        }
        if (colum != m - 1) {//Adyacente de la derecha
            ver_candy(mat, n, m, colum + 1, fila, vector,elemento);
        }
        
    }    
}

//obtencion de cuantas posiciones han sido encontradas para ser borradas
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
__global__ void matriz_aleatoria(int* mat, int n, int m, int lim_inf, int lim_sup, unsigned int ale, curandState* state) {
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

    // Calcular el índice global del hilo
    int id = idy * m + idx;

    // Calcular el índice local del hilo dentro del bloque
    int loc_id = threadIdx.y * blockDim.x + threadIdx.x;

    // Declarar la matriz compartida
    __shared__ int sh_mat[SH_DIM_Y * SH_DIM_X];

    // Inicializar la matriz compartida
    if (loc_id < SH_DIM_X * SH_DIM_Y) {
        int sh_id = (threadIdx.y + 1) * SH_DIM_X + threadIdx.x + 1;
        if (idx >= m || idy >= n) {
            sh_mat[sh_id] = 0;
        }
        else {
            sh_mat[sh_id] = mat[id];
        }
    }

    // Sincronizar los hilos dentro del bloque para asegurar que la matriz compartida se inicializa correctamente
    __syncthreads();

    // Verificar si el hilo se encuentra dentro de los límites de la matriz
    if (idx < m && idy < n && mat[id] == -1) {
        // Inicializar el generador de números aleatorios
        curand_init(ale, id, 0, &state[id]);

        // Generar un número aleatorio entero entre "lim_inf" y "lim_sup"
        int val = curand(&state[id]) % lim_sup + lim_inf;

        // Asignar el valor aleatorio al hueco
        sh_mat[(threadIdx.y + 1) * SH_DIM_X + threadIdx.x + 1] = val;

        // Sincronizar los hilos dentro del bloque para asegurar que todos los valores aleatorios se han generado antes de escribirlos en la matriz global
        __syncthreads();

        // Asignar los valores aleatorios generados a la matriz global
        mat[id] = sh_mat[(threadIdx.y + 1) * SH_DIM_X + threadIdx.x + 1];
    }
}


__global__ void eliminar_iguales_juntos(int* mat, int n, int m, int* vector) {
    // Definir la memoria compartida
    __shared__ int s_vector[SH_DIM_X * SH_DIM_Y];

    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    int tid = threadIdx.x + threadIdx.y * blockDim.x;

    // Copiar el vector a la memoria compartida
    if (tid < n * m) {
        s_vector[tid] = vector[tid];
    }
    __syncthreads();

    // Verificar si el hilo se encuentra dentro de los límites de la matriz y coincide con una posición que hay que eliminar
    if (idx < m && idy < n) {
        for (int i = 0; i < n * m; i++) {
            if (s_vector[i] == idy * m + idx) {
                mat[idy * m + idx] = -1;
                vector[i] = -1;
                break;
            }
        }
    }
}


__global__ void eliminar5(int* mat, int n, int m, int* vector,int fila,int columna) {
    // Definir la memoria compartida
    __shared__ int s_vector[SH_DIM_X * SH_DIM_Y];

    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    int tid = threadIdx.x + threadIdx.y * blockDim.x;

    bool centinela = false;
    int pos = fila * m + columna;
    // Copiar el vector a la memoria compartida
    if (tid < n * m) {
        s_vector[tid] = vector[tid];
    }
    __syncthreads();

    if (s_vector[0] == idy * m + idx) {
        mat[vector[0]] = 7;//Se pone una bomba en la posicion
        vector[0] = -1;
    }

    for (int i = 1; i < n * m; i++) {
        if (s_vector[i] == idy * m + idx) {
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

__global__ void eliminar6(int* mat, int n, int m, int* vector, int fila, int columna) {
    // Definir la memoria compartida
    __shared__ int s_vector[SH_DIM_X * SH_DIM_Y];

    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    int tid = threadIdx.x + threadIdx.y * blockDim.x;

    bool centinela = false;
    int pos = fila * m + columna;

    // Copiar el vector a la memoria compartida
    if (tid < n * m) {
        s_vector[tid] = vector[tid];
    }
    __syncthreads();

    if (s_vector[0] == idy * m + idx) {
        mat[fila * m + columna] = 8;//se pone una TNT en la posicion 
        s_vector[0] = -1;
    }

    for (int i = 1; i < n * m; i++) {
        if (s_vector[i] == idy * m + idx) {
            centinela = true; // El número está presente en el vector
            pos = i;
        }
    }

    // Verificar si el hilo se encuentra dentro de los límites de la matriz y coincide con una posición que hay que eliminar
    if (centinela) {
        mat[idy * m + idx] = -1;
        s_vector[pos] = -1;
    }

    // Copiar el vector de vuelta a la memoria global
    if (tid < n * m) {
        vector[tid] = s_vector[tid];
    }
}

__global__ void eliminar7oMas(int* mat, int n, int m, int* vector, int fila, int columna, unsigned int ale, curandState* state, int lim_sup, int lim_inf) {
    // Declarar la memoria compartida
    extern __shared__ int shared_mem[];
    int* mat_shared = shared_mem;
    int* vector_shared = &mat_shared[n * m];

    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;
    int tid = threadIdx.x + threadIdx.y * blockDim.x;

    // Copiar los datos a la memoria compartida teniendo en cuenta que no estén fuera de rango
    if (idx >= 0 && idx < m && idy >= 0 && idy < n) {
        mat_shared[idy * m + idx] = mat[idy * m + idx];
    }

    if (tid < n * m) {
        vector_shared[tid] = vector[tid];
    }
    
    __syncthreads();

    curand_init(ale, idy * m + idx, 0, &state[idy * m + idx]);

    // Generar un número aleatorio entero entre "lim_inf" y "lim_sup"
    int val_ale = curand(&state[idy * m + idx]) % lim_sup + lim_inf;

    //generar numero aleatorio que indicará que tipo de r se forma
    if (vector_shared[0] == idy * m + idx) {//9-->R1 10-->R2 11-->R3 21-->R4 13-->R5 14-->R6
        if (val_ale == 1) {//En la posicion se pone un R1
            mat_shared[idy * m + idx] = 9;
        }
        else if (val_ale == 2) {//En la posicion se pone un R2
            mat_shared[idy * m + idx] = 10;
        }
        else if (val_ale == 3) {//En la posicion se pone un R3
            mat_shared[idy * m + idx] = 11;
        }
        else if (val_ale == 4) {//En la posicion se pone un R4
            mat_shared[idy * m + idx] = 12;
        }
        else if (val_ale == 5) {//En la posicion se pone un R5
            mat_shared[idy * m + idx] = 13;
        }
        else {//En la posicion se pone un R6
            mat_shared[idy * m + idx] = 14;
        }
        vector_shared[0] = -1;
    }

    __syncthreads();

    for (int i = 1; i < n * m; i++) {
        if (vector_shared[i] == idy * m + idx) {// El número está presente en el vector
            mat_shared[idy * m + idx] = -1;
            vector_shared[i] = -1;
        }
    }

    // Copiar los datos actualizados de vuelta a la memoria global
    if (idx >= 0 && idx < m && idy >= 0 && idy < n) {
        mat[idy * m + idx] = mat_shared[idy * m + idx];
    }
    if (tid < n * m) {
        vector[idx] = vector_shared[idx];
    }
    
    __syncthreads();
}


__global__ void caer_caramelos(int* matriz, int n, int m) {
    // Declarar la memoria compartida
    extern __shared__ int shared_mem[];
    int* matriz_shared = shared_mem;

    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;

    // Copiar los datos a la memoria compartida
    matriz_shared[idy * m + idx] = matriz[idy * m + idx];
    __syncthreads();

    // Contar los elementos -1 debajo del hilo
    int num_minus_1 = 0;
    for (int i = idy; i < n; ++i) {
        if (matriz_shared[i * m + idx] == -1) {
            num_minus_1++;
        }
    }

    __syncthreads();
    // Buscar el primer elemento -1 debajo del hilo y intercambiarlo
    if (num_minus_1 > 0 && matriz_shared[idy * m + idx] != -1) {
        int aux = matriz_shared[idy * m + idx];
        matriz_shared[idy * m + idx] = -1;
        matriz_shared[(idy + num_minus_1) * m + idx] = aux;
    }

    __syncthreads();

    // Copiar los datos actualizados de vuelta a la memoria global
    matriz[idy * m + idx] = matriz_shared[idy * m + idx];
}




void caer_caramelos_host(int* matriz, int n, int m) {
    //Creacion de puntero para la GPU
    int* d_matriz;

    //Reservamos memoria 
    int size = n * m * sizeof(int);
    cudaMalloc((void**)&d_matriz, size);

    //copiamos del host al device
    cudaMemcpy(d_matriz, matriz, size, cudaMemcpyHostToDevice);

    // Configurar la cantidad de hilos por bloque y la cantidad de bloques por cuadrícula
    dim3 block_size(HILOS_BLOQUE_X, HILOS_BLOQUE_Y);
    dim3 num_blocks(BLOQUES_GRID_X, BLOQUES_GRID_Y);

    // Llamar al kernel caer_caramelos
    caer_caramelos <<<num_blocks, block_size >> > (d_matriz, n, m);

    // Copiar la matriz resultante de la GPU al host
    cudaMemcpy(matriz, d_matriz, size, cudaMemcpyDeviceToHost);

    // Liberar memoria de la GPU
    cudaFree(d_matriz);
}


__global__ void rellenar(int* vec, int n,int m) {
    //rellenar el vector con -1

    //posicion del hilo correspondiente al vector
    int idx = threadIdx.x + blockIdx.x * blockDim.x;
    int idy = threadIdx.y + blockIdx.y * blockDim.y;
    if (idy < n && idx < m) {
        vec[idy*m+idx] = -1;
    }
}

void crear_vector(int* posicionesVistas, int n, int m) {
    //creacion de un puntero para la GPU
    int* d_v;
    //Reservamos memoria
    cudaMalloc((void**)&d_v, n * m * sizeof(int));

    // Definir la configuración del kernel 
    dim3 block_size(HILOS_BLOQUE_X, HILOS_BLOQUE_Y);
    dim3 num_blocks(BLOQUES_GRID_X, BLOQUES_GRID_Y);

    // Llamar al kernel
    rellenar <<<num_blocks, block_size >> > (d_v, n, m);

    //Copiamos en el host el vector creado por el kernel
    cudaMemcpy(posicionesVistas, d_v, n * m * sizeof(int), cudaMemcpyDeviceToHost);
    //liberamos el puntero
    cudaFree(d_v);
}



void crear_matriz_aleatoria(int* mat, int n, int m, int lim_inf, int lim_sup) {
    //creacion de un puntero para la GPU
    int* d_mat;
    //reservamos espacio en la memoria
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));

    //puntero que ayudara a la obtencion de numeros aleatorios dentro del Kernel
    curandState* d_state;
    //reservamos espacio en la memoria
    cudaMalloc((void**)&d_state, n * m * sizeof(curandState));

    //obtencion de una semilla que ayudara a la creacion de numeros aleatorios
    unsigned int ale = generate_seed();

    dim3 block_size(HILOS_BLOQUE_X, HILOS_BLOQUE_Y);
    dim3 num_blocks(BLOQUES_GRID_X, BLOQUES_GRID_Y);
    matriz_aleatoria <<<num_blocks, block_size >> > (d_mat, n, m, lim_inf, lim_sup, ale, d_state);
    
    //Copiamos en el host el resultado obtenido en el device
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    //liberacion de punteros
    cudaFree(d_mat);
    cudaFree(d_state);
}

void rellenar_huecos_host(int* mat, int n, int m, int lim_inf, int lim_sup) {
    //creacion de un puntero para la GPU
    int* d_mat;

    //Reservamos memoria 
    cudaMalloc((void**)&d_mat, n * m * sizeof(int));

    //Copiamos del host al device 
    cudaMemcpy(d_mat, mat, n * m * sizeof(int), cudaMemcpyHostToDevice);

    //Creacion de un puntero que ayudara a la creacion de numeros aleatorios
    curandState* d_state;
    cudaMalloc((void**)&d_state, n * m * sizeof(curandState));

    unsigned int ale = generate_seed();

    dim3 block_size(HILOS_BLOQUE_X, HILOS_BLOQUE_Y);
    dim3 num_blocks(BLOQUES_GRID_X, BLOQUES_GRID_Y);
    rellenar_huecos <<<num_blocks, block_size >> > (d_mat, n, m, lim_inf, lim_sup, ale, d_state);

    //Copiamos del device al host
    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    //liberamos punteros
    cudaFree(d_mat);
    cudaFree(d_state);
}


__global__ void explotarBomba(int* mat, int n, int m, int fila, int columna, int tipo, int* vector) {
    int idx = threadIdx.x;
    int idy = threadIdx.y;

    if (tipo == 0) {//Eliminar la columna entera
        if (idx == columna) {
            mat[idy * m + idx] = -1;
        }
    }
    else {//Eliminar la fila entera
        if (idy == fila) {
            mat[idy * m + idx] = -1;
        }
    }
    //el elemento deja de ser analizado
    vector[0] = -1;
}


__global__ void explotarTNT(int* mat, int n, int m, int fila, int columna, int* vector) {
    int idx = threadIdx.x + blockDim.x * blockIdx.x;//coordenada de la fila del hilo
    int idy = threadIdx.y + blockDim.y * blockIdx.y;//coordenada de la columna del hilo

    //Comprobacion de que la posicion del hilo esta dentro de la matriz
    if (idx >= 0 && idx < m && idy >= 0 && idy < n) {
        //Comprobacion de que la posicion del hilo este dentro del radio de explosion
        if ((idx >= columna - 4 && idx <= columna + 4) && (idy >= fila - 4 && idy <= fila + 4)) {
            mat[idy * m + idx] = -1;
        }
    }

    //el elemento deja de ser analizado
    vector[0] = -1;
}

__global__ void explotarRx(int* matriz, int n, int m, int fila, int columna, int tipo,int *vector) { 
    int idx = threadIdx.x + blockDim.x * blockIdx.x;//coordenada de la fila del hilo
    int idy = threadIdx.y + blockDim.y * blockIdx.y;//coordenada de la columna del hilo
    int pos = idy * m + idx;//posicion del hilo
    matriz[fila*m+columna] = -1;
    switch (tipo) {
        case 9://R1-->eliminacion de todos los 1 de la matriz
            if (matriz[pos] == 1) {
                matriz[pos]=-1;
            }
            break;
        case 10://R2-->eliminacion de todos los 2 de la matriz
            if (matriz[pos] == 2) {
                matriz[pos] = -1;
            }
            break;
        case 11://R3-->eliminacion de todos los 3 de la matriz
            if (matriz[pos] == 3) {
                matriz[pos] = -1;
            }
            break;
        case 12://R4-->eliminacion de todos los 4 de la matriz
            if (matriz[pos] == 4) {
                matriz[pos] = -1;
            }
            break;
        case 13://R5-->eliminacion de todos los 5 de la matriz
            if (matriz[pos] == 5) {
                matriz[pos] = -1;
            }
            break;
        case 14://R6-->eliminacion de todos los 6 de la matriz
            if (matriz[pos] == 6) {
                matriz[pos] = -1;
            }
            break;
        default:
            break;
    }
    //el elemento deja de ser analizado
    vector[0] = -1;
}

__global__ void eliminar1(int* mat, int* vector, int n, int m, int fila, int columna) {
    int idx = threadIdx.x + blockDim.x * blockIdx.x;//coordenada de la fila del hilo
    int idy = threadIdx.y + blockDim.y * blockIdx.y;//coordenada de la columna del hilo
    int pos = idy * m + idx;//posicion del hilo
    if (pos == fila * m + columna) {
        vector[0] = -1;
        mat[pos] = -1;
    }
}

void eliminar_elementos(int* matriz, int n, int m, int* vector, int fila, int columna,int lim_sup, int lim_inf) {                       //AQUI HAY Q LLAMAR BIEN A LOS MÉTODOS DEL KERNEL
    //creacion de punteros para la GPU
    int* d_matriz;
    int* d_vector;

    int tamVector = n * m;
    srand(time(NULL));

    // Alocar memoria para la matriz y el vector en la GPU
    cudaMalloc((void**)&d_matriz, n * m * sizeof(int));
    cudaMalloc((void**)&d_vector, tamVector * sizeof(int));

    // Copiar la matriz y el vector de la CPU a la GPU
    cudaMemcpy(d_matriz, matriz, n * m * sizeof(int), cudaMemcpyHostToDevice);
    cudaMemcpy(d_vector, vector, tamVector * sizeof(int), cudaMemcpyHostToDevice);

    // Definir la configuración del kernel
    dim3 block_size(HILOS_BLOQUE_X, HILOS_BLOQUE_Y);
    dim3 num_blocks(BLOQUES_GRID_X, BLOQUES_GRID_Y);

    // Llamar al kernel
    switch (cuantas_posiciones(vector, n, m)) {
        //Comprobamos cuantas posiciones adyacentes hay
        case 1: 
            //No hay posciones adyacentes
            if (matriz[vector[0]] == 1 || matriz[vector[0]] == 2 || matriz[vector[0]] == 3 || matriz[vector[0]] == 4 || matriz[vector[0]] == 5 || matriz[vector[0]] == 6) {
                vidas--;
                eliminar1 << <num_blocks, block_size >> > (d_matriz, d_vector, n, m, fila, columna);
            }
            else if (matriz[vector[0]] == 7) {
                //BOMBA
                //generamos la semilla para luego crear un número aleatorio
                int ale = rand() % 2;
                explotarBomba <<<num_blocks, block_size >> > (d_matriz, n, m, fila, columna, ale,d_vector);
            }
            else if (matriz[vector[0]] == 8) {
                //TNT
                explotarTNT <<<num_blocks, block_size >> > (d_matriz, n, m, fila, columna,d_vector);
            }
            else if (matriz[vector[0]] > 8) {
                //Rx
                int tipo = matriz[vector[0]];
                explotarRx <<<num_blocks, block_size >> > (d_matriz, n, m, fila, columna, tipo,d_vector);
            }
            break;
        //Hay 1 caramelo adyacente
        case 2:
            eliminar_iguales_juntos <<<num_blocks, block_size >> > (d_matriz, n, m, d_vector);
            break;
        //Hay 2 caramelos adyacentes
        case 3:
            eliminar_iguales_juntos <<<num_blocks, block_size >> > (d_matriz, n, m, d_vector);
            break;
        //Hay 3 caramelos adyacentes
        case 4:
            eliminar_iguales_juntos <<<num_blocks, block_size >> > (d_matriz, n, m, d_vector);
            break;
        //Hay 4 caramelos adyacentes (se boorran 5 elementos-->bomba)
        case 5:
            //Kernel sustituir el elemento de la posición por un B y borrar el resto
            eliminar5 <<<num_blocks, block_size >> > (d_matriz, n, m, d_vector, fila, columna);
            break;
        //Hay 5 caramelos adyacentes (se boorran 6 elementos-->TNT)
        case 6:
            //Kernel sustituir el elemento de la posición por un TNT y borrar el resto
            eliminar6 <<<num_blocks, block_size >> > (d_matriz, n, m, d_vector, fila, columna);
            break;
        //Hay 6 o mas caramelos adyacentes (se boorran 7 o mas elementos-->Rx)
        default:
            //Kernel sustituir el elemento de la posición por un Rx y borrar el resto
            int tipo = rand() % lim_sup+lim_inf;
            //generamos la semilla para luego crear un número aleatorio
            curandState* d_state;
            cudaMalloc((void**)&d_state, n * m * sizeof(curandState));
            unsigned int ale = generate_seed();
            eliminar7oMas <<<num_blocks, block_size >> > (d_matriz, n, m, d_vector, fila, columna, ale, d_state,lim_sup,lim_inf);
            break;
        }


    // Copiar la matriz resultante de la GPU a la CPU
    cudaMemcpy(matriz, d_matriz, n * m * sizeof(int), cudaMemcpyDeviceToHost);
    cudaMemcpy(vector, d_vector, n * m * sizeof(int), cudaMemcpyDeviceToHost);
    // Liberar la memoria de la GPU
    cudaFree(d_matriz);
    cudaFree(d_vector);
}


//devuelve el minimo entre a y b
int minimo(int a, int b) {
    return (a < b) ? a : b;
}

//Calcula las mejores características para una mejor optimizacion
void mejoresCaracteristicas(int n, int m) {
    cudaDeviceProp deviceProp;
    cudaGetDeviceProperties(&deviceProp, 0);

    // Calculamos el número máximo de bloques por multiprocesador
    int max_blocks_per_sm = deviceProp.maxBlocksPerMultiProcessor;

    // Calculamos el número máximo de hilos por multiprocesador
    int max_threads_per_sm = deviceProp.maxThreadsPerMultiProcessor;

    // Calculamos el número de hilos por bloque
    int hilos_por_bloque_x = minimo(m, max_threads_per_sm);
    int hilos_por_bloque_y = minimo(n, max_threads_per_sm);

    // Calculamos el número de bloques por dimensión
    int bloques_por_dim_x = ceil((float)m / hilos_por_bloque_x);
    int bloques_por_dim_y = ceil((float)n / hilos_por_bloque_y);

    // Limitamos el número de bloques a lanzar por multiprocesador
    int bloques_por_sm = max_blocks_per_sm / (bloques_por_dim_x * bloques_por_dim_y);

    // Calculamos el número de bloques a lanzar
    int num_bloques = bloques_por_dim_x * bloques_por_dim_y;
    if (hilos_por_bloque_x * hilos_por_bloque_y > max_threads_per_sm) {
        num_bloques = minimo(num_bloques, bloques_por_sm * deviceProp.multiProcessorCount);
        hilos_por_bloque_x = minimo(m, max_threads_per_sm / hilos_por_bloque_y);
        hilos_por_bloque_y = minimo(n, max_threads_per_sm / hilos_por_bloque_x);
        bloques_por_dim_x = ceil((float)m / hilos_por_bloque_x);
        bloques_por_dim_y = ceil((float)n / hilos_por_bloque_y);
    }

    // Asignamos los valores finales a las variables globales
    BLOQUES_GRID_X = bloques_por_dim_x;
    BLOQUES_GRID_Y = bloques_por_dim_y;
    HILOS_BLOQUE_X = hilos_por_bloque_x;
    HILOS_BLOQUE_Y = hilos_por_bloque_y;

    printf("\n(Hilos/Bloque).x: %d\n", HILOS_BLOQUE_X);
    printf("\n(Hilos/Bloque).y: %d\n", HILOS_BLOQUE_Y);
    printf("\n(Bloques/Grid).x: %d\n", BLOQUES_GRID_X);
    printf("\n(Bloques/Grid).y: %d\n", BLOQUES_GRID_Y);
}

//imprimir la matriz
void imprimir(int* matriz, int n, int m) {
    char str[10];
    printf("\n");
    for (int i = -1; i < n; i++) {
        for (int j = -1; j < m; j++) {
            if (i == -1 && j == -1) {
                printf("_");
            }
            else if (i == -1 && j >= 0) {
                printf("|");
                printf("\x1b[4m%d\x1b[0m", j);
                printf("|");
                printf(" ");
            }
            else if (i != -1 && j == -1) {
                printf("\x1b[4m%d\x1b[0m", i);
                printf("|");
            }
            else if (matriz[i * m + j] == -1) {
                printf("    "); //elemento borrado, no se pone 
            }
            else if (matriz[i * m + j] == 1) {
                sprintf(str, "%d", matriz[i * m + j]);
                printf("\x1b[36m%s   \x1b[0m", str);//caramelo azul, 1
            }
            else if (matriz[i * m + j] == 2) {
                sprintf(str, "%d", matriz[i * m + j]);
                printf("\x1b[31m%s   \x1b[0m", str);//caramelo rojo, 2
            }
            else if (matriz[i * m + j] == 3) {
                sprintf(str, "%d", matriz[i * m + j]);
                printf("\x1b[38;5;226m%s   \x1b[0m", str); //caramelo naranja, 3
            }
            else if (matriz[i * m + j] == 4) {
                sprintf(str, "%d", matriz[i * m + j]);
                printf("\x1b[32m%s   \x1b[0m", str);// caramelo verde, 4
            }
            else if (matriz[i * m + j] == 5) {
                sprintf(str, "%d", matriz[i * m + j]);
                printf("\x1b[38;5;130m%s   \x1b[0m", str);//caramelo marron, 5
            }
            else if (matriz[i * m + j] == 6) {
                sprintf(str, "%d", matriz[i * m + j]);
                printf("\x1b[38;5;165m%s   \x1b[0m", str);//caramelo lila, 6
            }
            else if (matriz[i * m + j] == 7) {
                printf("\x1b[23;5;214mB   \x1b[0m");//Bomba
            }
            else if (matriz[i * m + j] == 8) {
                printf("\x1b[23;5;214mT   \x1b[0m");//TNT
            }
            else if (matriz[i * m + j] == 9) {
                printf("\x1b[36;5;214mR1  \x1b[0m");//R1
            }
            else if (matriz[i * m + j] == 10) {
                printf("\x1b[31;5;214mR2  \x1b[0m");//R2
            }
            else if (matriz[i * m + j] == 11) {
                printf("\x1b[38;5;226;5;214mR3  \x1b[0m");//R3
            }
            else if (matriz[i * m + j] == 12) {
                printf("\x1b[32;5;214mR4  \x1b[0m");//R4
            }
            else if (matriz[i * m + j] == 13) {
                printf("\x1b[38;5;130;5;214mR5  \x1b[0m");//R5
            }
            else if (matriz[i * m + j] == 14) {
                printf("\x1b[38;5;165;5;214mR6  \x1b[0m");//R6
            }
        }
        printf("\n");
    }
}


int main()
{
    srand(time(NULL));
    char tecla;
    int modo; //automático o manual
    int dificultad; //dificultad del juego
    int n; // número de filas
    int m; // número de columnas

  
    //Obtencion de valores de modo de juego
    printf("Bienvenido a Cundio Crack\n");
    printf("Introduce el modo de juego con el que quieres jugar: \n 1. Automatico \n 2. Manual \n");
    scanf("%d", &modo);
    printf("Introduce la dificultad con la que quieres jugar: \n 1. Facil \n 2. Normal \n");
    scanf("%d", &dificultad);
    printf("Introduce el numero de filas que quieres que tenga el tablero: \n");
    scanf("%d", &n);
    printf("Introduce el numero de columnas que quieres que tenga el tablero: \n");
    scanf("%d", &m);

    mejoresCaracteristicas(n, m);

    int lim_inf = 1; // valor mínimo
    int lim_sup = 6; // valor máximo
    if (dificultad == 1) {
         lim_sup = 4; // valor máximo
    }
    
    int* mat = (int*)malloc(n * m * sizeof(int)); // matriz aleatoria
    int* posicionesVistas = (int*)malloc(n * m * sizeof(int)); //Vector donde se guardan posiciones adyacentes
    crear_vector(posicionesVistas, n, m);//Inicializa el vector
    crear_matriz_aleatoria(mat, n, m, lim_inf, lim_sup);//Inicializacion de la matriz

    dim3 block_size(HILOS_BLOQUE_X, HILOS_BLOQUE_Y);
    dim3 num_blocks(BLOQUES_GRID_X, BLOQUES_GRID_Y);

    int colum=-1;
    int fila=-1;
    int x = 0;
    //Parte3
    printf("\n\x1b[31;5;214m%d VIDAS RESTANTES\x1b[0m\n", vidas);
    imprimir(mat, n, m);
    printf("\n");


    while (vidas > 0) {
        
        do {
            if (modo == 2) {
                //obtencion de filas y columnas por parte del usuario
                printf("\nIntroduce la fila del caramelo que quieres comprobar\n");
                scanf("%d", &fila);
                printf("Introduce la columna del caramelo que quieres comprobar\n");
                scanf("%d", &colum);
            }
            else { 
                //obtencion de filas y columna de manera aleatoria
                x++;
                srand(time(NULL) + x);
                colum =rand()%m; 
                x++;
                srand(time(NULL) + x);
                fila = rand()%n;

                printf("\nFila escogida: %d\n",fila);
                printf("Columna escogida: %d\n", colum);

                printf("\nPULSA ENTER PARA CONTINUAR\n");
                fflush(stdin);
                tecla = getchar();

            }
            if (colum > m - 1 || fila > n - 1 || colum < 0 || fila < 0) {
                printf("\nCOORDENADAS NO VALIDAS, introduce unas coordenadas posibles\n\n");
            }
        } while (colum > m-1 || fila > n-1 || colum < 0 || fila < 0);//comprobacion de que las filas introducidas no son validas
        
        int elemento = mat[fila * m + colum]; //caramelo en las coordenadas indicadas
        ver_candy(mat, n, m, colum, fila, posicionesVistas,elemento); //obtenemos un vector con todas los caramelos adyacentes al seleccionado, incluido el propio caramelo 
        printf("\n");

        eliminar_elementos(mat, n, m, posicionesVistas,fila,colum,lim_sup,lim_inf);//Eliminacion de la posicion seleccionada y sus adyacentesç
        imprimir(mat, n, m);
        caer_caramelos_host(mat, n, m);//caida de los caramelos que tengan elementos eliminados por debajo
        imprimir(mat, n, m);
        rellenar_huecos_host(mat, n, m, lim_inf, lim_sup);//donde haya elementos eliminados se ponen nuevos caramelos aleatorios

        if (vidas == 0)printf("\n\x1b[31;5;214mTE HAS QUEDADO SIN VIDAS\x1b[0m\n");
        else if (vidas > 1)printf("\n\x1b[31;5;214m%d VIDAS RESTANTES\x1b[0m\n", vidas);
        else printf("\n\x1b[31;5;214m%d VIDA RESTANTE\x1b[0m\n", vidas);

        imprimir(mat, n, m);
    }

    printf("\n-----Trabajo realizado por Jaime Diez Buendia y Eduardo Garcia Huerta-----\n");
    //liberacion del puntero 
    free(mat);

    //fin del programa
    return 0;
}