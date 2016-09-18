# __init__ � SGD � ������� �������.

# ������� ��������� (���������� = �������������)
sigmoid <- function(z) {
    return(1.0 / (1.0 + exp(-z)))
}

# ������ ������� ���������������
feedforward <- function(a, weights, biases) {
    # ���������� ����� ��������� ��� ������� ������ "a"
    for (i in seq_along(weights)) {
        a <- sigmoid(weights[[i]] %*% a + biases[[i]])
    }
    return(a) # �������, � �.�. 1�1, ���� ���� ������ �� ������
}

# ������ ��� ������� ����������� ��������� ����
cost_derivative <- function(output_activations, y) {
    # ����������� �� ������������ ������� ������
    return(output_activations - y)
}

# ������� ��� ������� ����������� ���������� ������� ���������
sigmoid_prime <- function(z) {
    return(sigmoid(z) * (1 - sigmoid(z)))
}

# ������� ��������� ��������������� ������
backprop <- function(model, x, y) {
    # x � y - ��������� ����������
    num_layers <- model$num_layers
    # ��������� (�������) �������� ��������
    nabla_b <- lapply(model$biases, 
                      function(x) rep(0, length(x)))
    nabla_w <- lapply(model$weights, 
                      function(x) matrix(rep(0, nrow(x) * ncol(x)), 
                                         nrow = nrow(x)))
    
    # feedforward
    activation <- x
    # ������ ��������� ��� ������� ���� 
    # (��������� ������� ���� - ��� ������� ������)
    activations <- list(x) 
    # ����������� ������ ��� ���������� ����
    zs <- list()
    
    # ��� ������� ����
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
        # ������� ����������, ����� ��������������� ���� �� ������
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


# ������� ��� ���������� ������ �� ������ ��������� �� ����-�����
update_mini_batch <- function(model, x, y, eta) {
    # ������ � ������� � ��������
    mini_batch <- list(x = x, y = y)
    
    # ��������� (�������) �������� ��������
    nabla_b <- lapply(model$biases, 
                      function(x) rep(0, length(x)))
    nabla_w <- lapply(model$weights, 
                      function(x) matrix(rep(0, nrow(x) * ncol(x)), 
                                         nrow = nrow(x)))
    
    # ��� ������� ����������
    for (i in 1:nrow(mini_batch$x)) {
        # ������������ ��������� ����� � �������� 
        # (���������� ��������� ������� ������)
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
    
    # ��������� ���� � �������� ������
    # �������� �������� ����� �� ���������� ���������� � �����
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
    
    test_data <- training_data # ��� ��������
    # �� ������ __init__:
    
    # ���������� ����� (������� ���� ������� ������)
    num_layers <- length(sizes)
    # ������������� �������� ���������� ����������
    biases <- lapply(sizes[-1], rnorm)
    # ������������� ����� ���������� ����������
    weights <- Map(function(x, y) matrix(rnorm(x*y), nrow = y), 
                   sizes[-length(sizes)], 
                   sizes[-1])
    # ������ � ����������� ������
    model <- list(biases = biases, weights = weights, num_layers = num_layers)
    # �� ������ SGD:
    
    # ���������� ���������� � �������� �������
    # if (! is.null(test_data)) n_test = nrow(test_data)
    
    # ���������� ���������� � ��������� �������
    n = nrow(training_data)
    
    # ��������� �������� ��� ������ �����
    for (j in 1:epochs) {
        # ������������ ����������
        ind <-  sample(x = 1:n, size = n, replace = FALSE)
        training_data <- training_data[ind, ]
        training_response <- training_response[ind, ,
                                               drop = FALSE ]
        
        # ����� �� ����-�����
        mini_batch_ind <- seq(1, n, mini_batch_size)
        # min(x + mini_batch_size, n) ����� �� ����� �� ������� �������
        mini_batch_data <- lapply(mini_batch_ind, 
                                  function(x) training_data[x:min(x + 
                                              mini_batch_size, n), ])
        mini_batch_response <- lapply(mini_batch_ind, 
                                      function(x) training_response[x:min(x + 
                                                  mini_batch_size, n), ,
                                                  drop = FALSE])
                                       # y ����� ���� �������� - ���. drop = FALSE
        
        # ������ � ������������ � �������� (� ���� ������)
        mini_batches <- list(x =  mini_batch_data, y = mini_batch_response)
        
        # ��������� ������ �� ������ ���������� �� ����-������
        for (i in seq_along(mini_batches$x)) {
            model <- update_mini_batch(model = model,
                                       x = mini_batches$x[[i]],
                                       y = mini_batches$y[[i]],
                                       eta = eta)
        }
        
        print(paste("�����", j, "���������"))
        # ������ �� �������� ������ ���� �� �����������
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
