# __init__ и SGD в главной функции.

# Функция активации (сигмоидная = логистическая)
sigmoid <- function(z) {
    return(1.0 / (1.0 + exp(-z)))
}

# Фунция прямого распространения
feedforward <- function(a, weights, biases) {
    # Возвращает ответ нейросети для входных данных "a"
    for (i in seq_along(weights)) {
        a <- sigmoid(weights[[i]] %*% a + biases[[i]])
    }
    return(a) # матрица, в т.ч. 1х1, если один нейрон на выходе
}

# Фунция для расчета производной выходного слоя
cost_derivative <- function(output_activations, y) {
    # Производная от квадратичной функции потерь
    return(output_activations - y)
}

# Функция для расчета производной сигмоидной функции активации
sigmoid_prime <- function(z) {
    return(sigmoid(z) * (1 - sigmoid(z)))
}

# Функция обратного распространения ошибки
backprop <- function(model, x, y) {
    # x и y - отдельные наблюдения
    num_layers <- model$num_layers
    # Начальные (нулевые) значения смещений
    nabla_b <- lapply(model$biases, 
                      function(x) rep(0, length(x)))
    nabla_w <- lapply(model$weights, 
                      function(x) matrix(rep(0, nrow(x) * ncol(x)), 
                                         nrow = nrow(x)))
    
    # feedforward
    activation <- x
    # Список активаций для каждого слоя 
    # (активация первого слоя - это входные данные)
    activations <- list(x) 
    # Аналогичные список для взвешенных сумм
    zs <- list()
    
    # Для каждого слоя
    for (i in seq_along(model$biases)) {
        z <- model$weights[[i]] %*% activation + model$biases[[i]]
        zs <- append(zs, list(z))
        activation <- sigmoid(z)
        activations <- append(activations, list(activation))
    }
    
    # backward pass
    delta <- cost_derivative(activations[[length(activations)]], y) *
             sigmoid_prime(zs[[length(zs)]])
        
    nabla_b[[length(nabla_b)]] <- delta

    nabla_w[[length(nabla_w)]] <- delta %*% 
                                  t(activations[[length(activations) - 1]])
    

    for (l in 1:(num_layers - 2)) {
        # Индексы исправлены, чтобы соответствовать коду на Питоне
        z <- zs[[length(zs) - l]]
        sp <- sigmoid_prime(z)
        delta <- (t(model$weights[[length(model$weights) - l + 1]]) %*%
                 delta) * sp
        nabla_b[[length(nabla_b) - l]] <- delta
        nabla_w[[length(nabla_w) - l]] <- delta %*%
            t(activations[[length(activations) - l - 1]]) 
    }
    nabla_b_w <- list(delta_nabla_b = nabla_b, delta_nabla_w = nabla_w)
    return(nabla_b_w)

}


# Функция для обновления модели на основе градиента по мини-батчу
update_mini_batch <- function(model, x, y, eta) {
    # Список с данными и ответами
    mini_batch <- list(x = x, y = y)
    
    # Начальные (нулевые) значения смещений
    nabla_b <- lapply(model$biases, 
                      function(x) rep(0, length(x)))
    nabla_w <- lapply(model$weights, 
                      function(x) matrix(rep(0, nrow(x) * ncol(x)), 
                                         nrow = nrow(x)))
    
    # Для каждого наблюдения
    for (i in 1:nrow(mini_batch$x)) {
        # Рассчитываем изменения весов и смещений 
        # (складываем градиенты функции потерь)
        delta_nabla_b_w <- backprop(model = model,
                                    x = mini_batch$x[i, ], 
                                    y = mini_batch$y[i, ])
        nabla_b <- Map(`+`, 
                       nabla_b,
                       delta_nabla_b_w$delta_nabla_b)
        nabla_w <- Map(`+`, 
                       nabla_w,
                       delta_nabla_b_w$delta_nabla_w)
    }
    
    # Обновляем веса и смещения модели
    # Скорость обучения делим на количество наблюдений в батче
    model$biases <- Map(function(b, nb) b - (eta / nrow(mini_batch$x)) * nb,
                        model$biases,
                        nabla_b)
    model$weights <- Map(function(w, nw) w - (eta / nrow(mini_batch$x)) * nw,
                         model$weights,
                         nabla_w)
    
    return(model)
}
 


   
network <- function(sizes, 
                    training_data,
                    training_response,
                    epochs, 
                    mini_batch_size, 
                    eta,
                    test_data = NULL,
                    test_response = NULL) {
    
    test_data <- training_data # для проверки
    # Из метода __init__:
    
    # Количество слоев (включая слой входных данных)
    num_layers <- length(sizes)
    # Инициализация смещений случайными значениями
    biases <- lapply(sizes[-1], rnorm)
    # Инициализация весов случайными значениями
    weights <- Map(function(x, y) matrix(rnorm(x*y), nrow = y), 
                   sizes[-length(sizes)], 
                   sizes[-1])
    # Список с параметрами модели
    model <- list(biases = biases, weights = weights, num_layers = num_layers)
    # Из метода SGD:
    
    # Количество наблюдений в тестовой выборке
    # if (! is.null(test_data)) n_test = nrow(test_data)
    
    # Количество наблюдений в обучающей выборке
    n = nrow(training_data)
    
    # Повторяем обучение для каждой эпохи
    for (j in 1:epochs) {
        # Перемешиваем наблюдения
        ind <-  sample(x = 1:n, size = n, replace = FALSE)
        training_data <- training_data[ind, ]
        training_response <- training_response[ind, ,
                                               drop = FALSE ]
        
        # Делим на мини-батчи
        mini_batch_ind <- seq(1, n, mini_batch_size)
        # min(x + mini_batch_size, n) чтобы не выйти за пределы таблицы
        mini_batch_data <- lapply(mini_batch_ind, 
                                  function(x) training_data[x:min(x + 
                                              mini_batch_size, n), ])
        mini_batch_response <- lapply(mini_batch_ind, 
                                      function(x) training_response[x:min(x + 
                                                  mini_batch_size, n), ,
                                                  drop = FALSE])
                                       # y может быть вектором - исп. drop = FALSE
        
        # Список с предикторами и ответами (в виде матриц)
        mini_batches <- list(x =  mini_batch_data, y = mini_batch_response)
        
        # Обновляем модель на основе градиентов по мини-батчам
        for (i in seq_along(mini_batches$x)) {
            model <- update_mini_batch(model = model,
                                       x = mini_batches$x[[i]],
                                       y = mini_batches$y[[i]],
                                       eta = eta)
        }
        
        print(paste("Эпоха", j, "завершена"))
        # Оценка на тестовых данных пока не реализована
    }
    result <- apply(test_data, 1, 
                    feedforward, 
                    weights = model$weights,
                    biases = model$biases)
    return(result)
}
    

training_data <- as.matrix(data.frame(x1 = c(1:100), x2 = c(2:101)))
training_response <- matrix(c(1:100 + 2:101 + rnorm(10)), nrow = 100)

training_data <- as.matrix(iris[, 1:4])
training_response <- predict(dummyVars(~ Species, data = iris), iris)

fit <- network(sizes = c(4, 8, 3), 
        training_data = training_data, 
        training_response = training_response,
        epochs = 30, 
        mini_batch_size = 10, 
        eta = 1,
        )

fit <- t(fit)
resp <- apply(fit, 1, function(x) which(x == max(x)))
resp
as.numeric(iris$Species)

