sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))
y <- 4
x <- 4
condition <- 4
condition_call <- 4


subset2 <- function(x, condition) {
   condition_call <- substitute(condition)
   r <- eval(condition_call, x)
   x[r, ]
 }

subset2(x = sample_df, condition = a == condition)

x <- sample_df
condition_call <- substitute(a == condition)

eval(condition_call, x)
Здесь функция eval() пытается выполнить выражение a == condition 
(оно присвоено переменной condition_call) в окружении x,
а x у нас является таблицой sample_df. Т.е. ожидается, что a и condition
будут именами столбцов.
Столбец с именем "a" будет найден, а столбец с именем "condition" - нет,
поэтому поиск соответствия этому имени переменной будет продолжен 
в вышестоящем окружении, а именно - в окружении выполнения функции subset2().
Уточнение: окружение выполнения является "виртуальным" окружением, которое 
создается в момент вызова функции и обычно исчезает после окончания ее работы
(но сохраняется при использовании замыканий, когда внутри функции создаются
новые функции).
Итак, в этом "виртуальном" окружении происходит присвоение переменным
значений аргументов, переданных функции, во время обращения к соответствующим
переменным ("ленивые" вычисления в действии): 
присвоение x <- sample_df и condition <- a == condition в нашем случае.
Тогда при обращении к переменной "condition" происходит попытка ее вычисления,
но выражение a == condition в данном окружении вычислить нельзя, поскольку
там нет переменной "а". Нет такой переменной и в еще более выщестоящих 
окружениях, в данном случае - в глобальном. Поэтому получаем ошибку.
Если создать в глобальном окружении переменную "a" (a <- 4), то функция
будет работать, но возращать совсем не то, что нужно. a == condition
в окружении, где a = 4 и condition = 4, вернет единственное значение TRUE,
поэтому будет выбрана только первая строка в таблице, что и наблюдаем.

http://www.hep.by/gnu/r-patched/r-lang/R-lang_75.html

# condition <- a == condition

substitute принимает объекты типа promise ("обещание"), в виде которых существуют 
переданные функции аргументы до их вычисления, т.е. если
передать функции аргумент condition = a == condition, то 
substitute(condition) обработает объект-"обещание" под названием condition,
который предсталяет собой выражение "a == condition",
и вернет невычисленное выражение "a == condition",
а quote(condition) просто вернет невычисленную переменную condition
Но  при непосредственной передаче выражения обе эти функции ведут себя одинаково:
substitute(a == condition) == quote(a == condition)
даст TRUE


https://www.quora.com/Could-someone-give-an-explanation-of-how-eval-quote-and-substitute-work-in-R

http://stackoverflow.com/questions/30563745/non-standard-evaluation-from-another-function-in-r

http://stackoverflow.com/questions/31479813/non-standard-evaluation-confusion-in-advanced-r-book