# Se presentan las funciones de interés simple
# Autor: Juan Andrés Pérez Raya
# V 1.0: 8 de diciembre de 2024

# Valor futuro de anualidades anticipadas
ValorFuturo=function(A,t,r){
  VF=A*((((1+r)^t)-1)/r)*(1+r)
  
  return(VF)
}

# Anualidad con valor futuro
AnualidadVF=function(VF,t,r){
  A=VF/(((((1+r)^t)-1)/r)*(1+r))
  
  return(A)
}

# Número de pagos con valor futuro
NumeroPagosVF=function(VF,A,r){
  t=log(((VF*r)/(A*(1+r)))+1)/log(1+r)
  
  return(t)
}

# Tasa del periodo con valor futuro
TasaPeriodoVF= function(VF,A,t){
  func = function(r) {
    LadoIzquierdo = VF / A
    LadoDerecho = (((1 + r) ^ t - 1) / r)*(1+r)
    return(LadoIzquierdo - LadoDerecho)
  }
  
  
  resultado = uniroot(func, lower = 0.0000001, upper = 1)
  
  
  r= resultado$root
  return(r)
  
}

# Valor actual de anualidades anticipadas 
ValorActual=function(A,t,r){
  VA=A*((1-(1+r)^-t)/r)*(1+r)
  
  return(VA)
}

# Anualidad con valor actual
AnualidadVA=function(VA,t,r){
  A=VA/(((1-((1+r)^-t))/r)*(1+r))
  
  return(A)
}

# Número de pagos con valor actual
NumeroPagosVA=function(VA,A,r){
  t=-log(1-((VA*r)/(A*(1+r))))/log(1+r)
  
  return(t)
}

# Tasa del periodo con valor actual
TasaPeriodoVA= function(VA,A,t){
  func2 = function(r) {
    LadoIzquierdo = VA / A
    LadoDerecho = ((1-((1+r)^-t))/r)*(1+r)
    return(LadoIzquierdo - LadoDerecho)
  }
  
  
  resultado = uniroot(func2, lower = 0.000001, upper = 1)
  
  
  r= resultado$root
  return(r)
}
