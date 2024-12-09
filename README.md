# Anualidades-Anticipadas

# Compilación de Funciones de Anualidades Anticipadas
En este repositorio se ilustran las fórmulas de anualidades anticipadas para la materia de matemáticas financieras.

## Funciones de Anualidades Vencidas

Con el siguiente código, puede usted cargar las funciones que se emplean para realizar los cálculos correspondientes del interés simple.
```
source("https://raw.githubusercontent.com/Jandres17/Anualidades-Anticipadas/refs/heads/main/FormulasAnualidadesAnticipadas.R")
```

A continuación se ilustrarán algunos ejemplos de las fórmulas correspondientes.

## 1. Cálculo de Valor Futuro conociendo la anualidad, tasa de interés del periodo y el número (plazo) de anualidades.

Un inversionista realiza un pago mensual de $1,200 al final de cada mes durante 5 años en una anualidad anticipada que paga una tasa de interés del 6% anual, compuesta mensualmente. ¿Cuál es el valor futuro de esta inversión?
Los datos son los siguientes:
A = $1,200.00
i = 6.00% anualizado
r = 0.50% mensual
t = 60 meses

Se realizan los cálculos de VF
```
VF = ValorFuturo(A=1200, r=0.005, Tper=60)
```

## 2. Cálculo de Anualidad conociendo valor futuro, tasa del periodo y número de pagos.

Una persona desea acumular $200,000 en 15 años mediante pagos mensuales bajo una anualidad anticipada con una tasa de interés del 3% anual, compuesta mensualmente. ¿Cuánto debe pagar cada mes?
Se tienen los siguientes datos:
VF = $200,000.00 
i = 3.00% anualizado
r = 0.25% mensual
t = 180 meses

Se realizan los cálculos de A
```
A = AnualidadVF(VF=200000, r=0.0025, Tper=180)
```

## 3. Cálculo de Número de Pagos o Plazo, conociendo valor futuro, número de pagos y tasa del periodo.

Un individuo realiza pagos mensuales de $2,500 al inicio de cada mes en una cuenta que ofrece una tasa de interés del 4.5% anual, compuesta mensualmente. Si desea acumular $180,000, ¿cuántos meses debe realizar estos pagos bajo una anualidad vencida?
Se tienen los siguientes datos:
A = $2,500.00
i = 4.50% anualizado
r = 0.375%
VFt= $180,000.00

Se realizan los cálculos de Tper
```
T = NumeroPagosVF(VF=180000, A=2500, r=0.00375)
```

## 4. Cálculo de Tasa del periodo conociendo valor futuro, número de pagos y monto de la anualidad.

Se realizan pagos mensuales de $1,500 al inicio de cada mes durante 7 años bajo una anualidad anticipada. Si al final se acumula un valor futuro de $150,000, ¿cuál es la tasa de interés anual compuesta mensualmente?
Se tienen los siguientes datos:
A = $1,500.00
T = 84 meses
VFt = $150,000.00

Se realizan los cálculos de tasaper
```
r = TasaPeriodoVF(VF=150000, A=1500, Tper=84)
```

## 5. Cálculo de Valor Actual conociendo la anualidad, tasa de interés del periodo y el número (plazo) de anualidades.

Suponga que una persona pide prestado para una nueva tableta para utilizarla en la escuela y desea pagarlos en 48 pagos quincenales de $66.41. Para esto la tienda departamental decide cobrarle una tasa de interés del 12% anualizada ¿A cuánto asciende el valor de la tableta adeudado?
Se tienen los siguientes datos:
A = $66.41 
T = 48 quincenas
i = 12.00% anualizada
r = 0.25% quincenal

Se realizan los cálculos de VA
```
VA = ValorActual(A=66.41, r=0.0025, Tper=48)
```

## 6. Cálculo de Anualidad conociendo valor actual, tasa del periodo y número de pagos.

Para ilustrar su empleo se utiliza el ejemplo anterior. Del mismo se conoce que el monto adeudado al comprar la tableda es de VAt=$3,000.00 se paga en T=48 quincenas a una tasa anualizada de i=12.00% (0.25% quincenal).
Se tienen los siguientes datos:
VAt = $3,000.00
T = 48 quincenas
i = 12.00% anualizada
r = 0.25% quincenal

Se realizan los cálculos de A
```
A = AnualidadVA(VA=3000, r=0.0025, Tper=48)
```

## 7. Cálculo de Número de Pagos o Plazo, conociendo valor actual, número de pagos y tasa del periodo.

De nueva cuenta, para exponer su uso, se retoma el ejercicio anterior (del que se sabe que T=48) pero ahora se supone que solo se conoce VAt=$3,000.00 r=0.25 (i=12.00%) y A=$66.41
Se tienen los siguientes datos:
VAt = $3,000.00
i = 12.00% anualizada
r = 0.25% quincenal
A = $66.41 

Se realizan los cálculos de t
```
t = NumeroPagosVA(VA=3000, A=66.41, r=0.0025)
```

## 8. Tasa del periodo, conociendo valor actual, número de pagos y monto de la anualidad.

Como se conoce del ejercicio, el monto adedudado es VAt=$3,000 el número de quincenas a pagar es de T=48 y el pago quincenal de A=$66.40. Para resolver el problema se va a utilizar el código de iteración que se generó para esta clase en R. 
Se tienen los siguientes datos:
VAt = $3,000.00
A = $66.41 
T = 48 quincenas

Se realizan los cálculos de r
```
r = TasaPeriodoVA(VA=3000, PMT=66.41, Tper=48)
```


