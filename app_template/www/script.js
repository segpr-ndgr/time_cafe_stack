


/**********************************************************************************/
/*                             SEÇÃO 1: DOCUMENT READY                            */
/**********************************************************************************/
/**
 * Executado quando o documento/carregamento da página estiver pronto.
 * Aqui definimos um contador (n) e um handler de clique para o elemento #auth-go_auth.
 * Ao clicar, incrementamos o contador e enviamos o valor para o Shiny.
 */
$(document).ready(function() {
  // Inicializa um contador
  var n = 0;
  
  // Cria um handler de clique que escuta cliques no elemento com id #auth-go_auth
  $("#auth-go_auth").on("click", function() {
    // Incrementa o contador cada vez que clicamos
    n++;
    // Envia mensagem (valor de n) para Shiny
    Shiny.setInputValue("lgnclick", n);
  });
});

/**********************************************************************************/
/*                            SEÇÃO 2: VARIÁVEIS GLOBAIS                          */
/**********************************************************************************/
/**
 * Dimensões atuais da janela (largura x altura).
 */
var dimension = [window.innerWidth, window.innerHeight];

/**
 * Flag para verificar se o painel lateral já foi "colapsado".
 * Usado para controlar o comportamento no redimensionamento da janela.
 */
let isCollapsed = false;

/**********************************************************************************/
/*                           SEÇÃO 3: FUNÇÕES UTILITÁRIAS                         */
/**********************************************************************************/
/**
 * Alterna classes de um elemento DOM.
 * @param {HTMLElement} x - Elemento sobre o qual togglar classes.
 * @param {string} add - Classe a ser adicionada.
 * @param {string} remove - Classe a ser removida.
 */
function mlkToggle(x, add, remove) {
  x.classList.toggle(remove);
  x.classList.toggle(add);
}

/**
 * Verifica se dois arrays têm os mesmos elementos (igual conjunto).
 * Observação: assume que os elementos do array são tipos primitivos.
 * @param  {Array} a1 - Primeiro array
 * @param  {Array} a2 - Segundo array
 * @returns {boolean} true se forem iguais, false caso contrário
 */
function areArraysEqualSets(a1, a2) {
  const superSet = {};
  for (const i of a1) {
    const e = i + typeof i;
    superSet[e] = 1;
  }
  for (const i of a2) {
    const e = i + typeof i;
    if (!superSet[e]) {
      return false;
    }
    superSet[e] = 2;
  }
  for (let e in superSet) {
    if (superSet[e] === 1) {
      return false;
    }
  }
  return true;
}

/**
 * Retorna uma Promise que resolve após 'ms' milissegundos.
 * @param  {number} ms - Tempo em milissegundos
 * @returns {Promise<void>}
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**********************************************************************************/
/*                        SEÇÃO 4: INTEGRAÇÃO COM SHINY                           */
/**********************************************************************************/
/**
 * Alterna o ícone de "toggle" (fa-toggle-on / fa-toggle-off) e manipula a exibição
 * de colunas em uma tabela Reactable.
 * @param {Object} message - Objeto contendo informações como IDs de elementos e colunas a esconder/exibir.
 */
function mlkToggleCard(message) {
  // Verifica se o ícone atual está com 'fa-toggle-on'
  var condition = document.getElementById(message.toggle_card_id).classList.contains('fa-toggle-on');
  
  // Envia o estado atual do card para o Shiny (true/false)
  Shiny.setInputValue(message.card_id, condition, { priority: 'event' }); 
  
  // Verifica flag retornada do Shiny
  var flag = Shiny.shinyapp.$inputValues[message.input_id];
  
  // Caso o card não esteja "on" e o Shiny indique "show"
  if ((!condition) && (flag == 'show')) {
    if (document.getElementById(message.id) === null) {
      setTimeout(() => {
        Reactable.setHiddenColumns(message.id, [].concat(message.cols_never));
      }, 100);
    }
    if (document.getElementById(message.id) === null) {
      setTimeout(() => {
        Reactable.setHiddenColumns(message.id, [].concat(message.cols_never));
      }, 200);
    }
    if (document.getElementById(message.id) === null) {
      setTimeout(() => {
        Reactable.setHiddenColumns(message.id, [].concat(message.cols_never));
      }, 200);
    }
  }
}

/**
 * Handler customizado do Shiny para alternar card/tabela.
 * Chama a função mlkToggleCard ao receber a mensagem 'switch-card-table'.
 */
Shiny.addCustomMessageHandler('switch-card-table', function(message) {
  mlkToggleCard(message);
});

/**
 * Esconde ou exibe colunas de uma tabela Reactable de acordo com o array de colunas desejadas.
 * @param {Array} x - Lista de colunas para esconder ou exibir
 * @param {Array} neverShow - Colunas que nunca devem ser exibidas
 * @param {string} table_id - ID da tabela (Reactable) no DOM
 * @param {string} input_id - ID do input no Shiny para receber o estado atual
 */
function mlkHideColumns(x, neverShow, table_id, input_id) {
  // Adiciona as colunas que nunca devem aparecer ao array x
  x.push.apply(x, neverShow);
  
  // Define colunas ocultas usando Reactable
  Reactable.setHiddenColumns(table_id, prevColumns => {
    if (areArraysEqualSets(x, prevColumns)) {
      // Se as colunas atuais já estão escondidas
      Shiny.setInputValue(input_id, "show", { priority: 'event' });
      return neverShow;
    } else {
      Shiny.setInputValue(input_id, "hide", { priority: 'event' });
      return x;
    }
  });
}

/**
 * Handler customizado do Shiny para mostrar/esconder colunas.
 * Chama a função mlkHideColumns ao receber a mensagem 'show-hide-values'.
 */
Shiny.addCustomMessageHandler('show-hide-values', function(message) {
  mlkHideColumns([].concat(message.cols), [].concat(message.cols_never), message.id, message.input_id);
});

/**
 * Envia um objeto com a propriedade 'x' para o Shiny, usando o ID 'button_id'.
 * @param {any} x - Dados a enviar
 * @param {string} button_id - ID do input no Shiny
 */
function mlkCardDetails(x, button_id) {
  if (window.Shiny) {
    Shiny.setInputValue(button_id, { x }, { priority: 'event' });
  }
}

/**
 * Envia um objeto com a propriedade 'x' para o Shiny, usando o ID 'button_id'.
 * @param {any} x - Dados a enviar
 * @param {string} button_id - ID do input no Shiny
 */
function mlkCardButton(x, button_id) {
  if (window.Shiny) {
    Shiny.setInputValue(button_id, { x }, { priority: 'event' });
  }
}

/**********************************************************************************/
/*                       SEÇÃO 5: FUNÇÕES DE SCROLL (IR PARA O TOPO)              */
/**********************************************************************************/
/**
 * Mostra ou esconde o botão de "voltar para o topo" baseado no scroll vertical.
 */
window.onscroll = function() {
  scrollFunction();
};

/**
 * Mostra/esconde o botão "topBtn" quando o scroll ultrapassa 20px.
 */
function scrollFunction() {
  let topbutton = document.getElementById("topBtn");
  if (document.body.scrollTop > 20 || document.documentElement.scrollTop > 20) {
    if (topbutton) topbutton.style.display = "block";
  } else {
    if (topbutton) topbutton.style.display = "none";
  }
}

/**
 * Quando o usuário clicar no botão, rola o documento de volta para o topo.
 */
function topFunction() {
  document.body.scrollTop = 0;
  document.documentElement.scrollTop = 0;
}

/**********************************************************************************/
/*                   SEÇÃO 6: REDIMENSIONAMENTO DA JANELA (COLAPSE)              */
/**********************************************************************************/
/**
 * Atualiza o array 'dimension' quando a janela é redimensionada.
 * Colapsa o painel (#collapsePainel) se a largura < 767 ou altura < 400 (e se ainda não estiver colapsado).
 * Restaura a flag quando a janela volta a ser maior que esses limites.
 */
window.addEventListener("resize", event => {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;

  if (!isCollapsed && (dimension[0] < 767 || dimension[1] < 400)) {
    $('#collapsePainel.collapse').collapse('hide');
    isCollapsed = true; // Marca que o colapso já ocorreu
  }

  // Se a janela voltou a ficar maior que 767x400, reseta a flag para permitir colapsar novamente
  if (dimension[0] >= 767 && dimension[1] >= 400) {
    isCollapsed = false;
  }
});

/**********************************************************************************/
/*                         SEÇÃO 7: EVENTOS DO SHINY                              */
/**********************************************************************************/
/**
 * Quando a conexão com Shiny é estabelecida, envia as dimensões atuais para o input "painel-dimension".
 */
$(document).on("shiny:connected", function() {
  Shiny.setInputValue("painel-dimension", dimension);
});

/**********************************************************************************/
/*                   SEÇÃO 8: EVENTOS DE CLIQUE (GATILHOS E TABS)                 */
/**********************************************************************************/
/**
 * Quando o elemento com id #gatilho_tce for clicado, dispara o clique no tab #tomeconta-tab.
 */
$(document).on('click', '#gatilho_tce', function() {
  $('#tomeconta-tab').trigger('click');
});

/**
 * Quando o elemento com id #gatilho_efisco for clicado, dispara o clique no tab #efisco-tab.
 */
$(document).on('click', '#gatilho_efisco', function(e) {
  e.preventDefault();
  $('#efisco-tab').trigger('click');
});

/**
 * Quando o elemento com id #gatilho_despesas for clicado, dispara o clique no tab #despesas-tab.
 */
$(document).on('click', '#gatilho_despesas', function() {
  $('#despesas-tab').trigger('click');
});

/**
 * Ao clicar em cada tab (tomeconta, efisco, despesas, anomalias),
 * colapsa o painel lateral (#collapsePainel).
 */
$(document).on('click', '#tomeconta-tab', function() {
  $('#collapsePainel.collapse').collapse('hide');
});
$(document).on('click', '#efisco-tab', function() {
  $('#collapsePainel.collapse').collapse('hide');
});
$(document).on('click', '#despesas-tab', function() {
  $('#collapsePainel.collapse').collapse('hide');
});
$(document).on('click', '#anomalias-tab', function() {
  $('#collapsePainel.collapse').collapse('hide');
});