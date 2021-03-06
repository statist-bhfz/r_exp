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
����� ������� eval() �������� ��������� ��������� a == condition 
(��� ��������� ���������� condition_call) � ��������� x,
� x � ��� �������� �������� sample_df. �.�. ���������, ��� a � condition
����� ������� ��������.
������� � ������ "a" ����� ������, � ������� � ������ "condition" - ���,
������� ����� ������������ ����� ����� ���������� ����� ��������� 
� ����������� ���������, � ������ - � ��������� ���������� ������� subset2().
���������: ��������� ���������� �������� "�����������" ����������, ������� 
��������� � ������ ������ ������� � ������ �������� ����� ��������� �� ������
(�� ����������� ��� ������������� ���������, ����� ������ ������� ���������
����� �������).
����, � ���� "�����������" ��������� ���������� ���������� ����������
�������� ����������, ���������� �������, �� ����� ��������� � ���������������
���������� ("�������" ���������� � ��������): 
���������� x <- sample_df � condition <- a == condition � ����� ������.
����� ��� ��������� � ���������� "condition" ���������� ������� �� ����������,
�� ��������� a == condition � ������ ��������� ��������� ������, ���������
��� ��� ���������� "�". ��� ����� ���������� � � ��� ����� ����������� 
����������, � ������ ������ - � ����������. ������� �������� ������.
���� ������� � ���������� ��������� ���������� "a" (a <- 4), �� �������
����� ��������, �� ��������� ������ �� ��, ��� �����. a == condition
� ���������, ��� a = 4 � condition = 4, ������ ������������ �������� TRUE,
������� ����� ������� ������ ������ ������ � �������, ��� � ���������.

http://www.hep.by/gnu/r-patched/r-lang/R-lang_75.html

# condition <- a == condition

substitute ��������� ������� ���� promise ("��������"), � ���� ������� ���������� 
���������� ������� ��������� �� �� ����������, �.�. ����
�������� ������� �������� condition = a == condition, �� 
substitute(condition) ���������� ������-"��������" ��� ��������� condition,
������� ����������� ����� ��������� "a == condition",
� ������ ������������� ��������� "a == condition",
� quote(condition) ������ ������ ������������� ���������� condition
��  ��� ���������������� �������� ��������� ��� ��� ������� ����� ���� ���������:
substitute(a == condition) == quote(a == condition)
���� TRUE


https://www.quora.com/Could-someone-give-an-explanation-of-how-eval-quote-and-substitute-work-in-R

http://stackoverflow.com/questions/30563745/non-standard-evaluation-from-another-function-in-r

http://stackoverflow.com/questions/31479813/non-standard-evaluation-confusion-in-advanced-r-book