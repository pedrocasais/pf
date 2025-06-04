#!/bin/bash

# Arquivo de testes
TEST_FILE="./a.out"

# Gerando os testes
cat <<EOL > $TEST_FILE
HCM  # Válido
HCACM  # Válido
HCCAM  # Válido
HCCACAM  # Válido
HAM  # Inválido
HACM  # Inválido
HCMA  # Inválido
CHM  # Inválido
HMC  # Inválido
HCCM  # Inválido
EOL

# Função para validar um input
validate() {
    input="$1"
    if [[ "$input" =~ ^H[C]+A*C+M$ ]]; then
        echo "YES"
    else
        echo "NO"
    fi
}

# Testando os casos
while read -r line; do
    case_input=$(echo "$line" | awk '{print $1}')
    validate "$case_input"
done < "$TEST_FILE"
