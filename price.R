

library(tidyverse)
library(ggthemes)


# dataset
readxl::read_excel("shopping_price.xlsx") -> data
read_csv("names.csv") -> data1


## 2021

readxl::read_excel("price_2021.xlsx") |> 
  janitor::clean_names() |> 
  dplyr::select(-nome, -id, -cidade, -hora_de_inicio, -telefone_da_revenda, 
                -informe_abaixo_o_valor_da_ponta_de_pulverizacao_ex_0_00_use_ponto, 
                -modelo_da_ponta_de_pulverizacao, 
                -caso_necessario_utilize_o_campo_abaixo_para_importantes_comentarios_sobre_posicionamento_de_preco_identificado_durante_a_coleta_de_informacoes_deste_formulario,
                -hora_de_conclusao, -email, -vendedor, -fabricante) |> 
  pivot_longer(cols = xr_vk:jmd, names_to = "pontas", values_to = "price",
               names_repair = "check_unique") |> 
  rename(estado = estado_informar_a_sigla_ex_sp,
         marcas = quais_marcas_de_pontas_de_pulverizacao_esta_revenda_comercializa) |> 
  filter(!is.na(price),
         price != 0) |> 
  mutate(price = str_remove(price, "[.]"),
         price = str_replace(price, "(^\\d{2})", "\\1."),
         price = round(as.double(price),2),
         pontas = str_replace(pontas, "_", "-"),
         pontas = str_to_upper(pontas),
         pontas = if_else(pontas == "TT", "TT-VP", pontas),
         pontas = if_else(pontas == "ADIA", "AD-IA", pontas))  -> dados_21




data1 |> 
  dplyr::select(manufacturer, family, group) -> group



dados_21 |> 
  left_join(group, by = c("pontas" = "family")) |> 
  mutate(ano = 2021) |> 
  distinct(estado, nome_da_revenda, pontas, price, .keep_all = TRUE) -> dados_2021


## 2022


readxl::read_excel("price_2022.xlsx", sheet = "Geral") |> 
  janitor::clean_names() |> 
  dplyr::select(-id, - hora_de_inicio, -hora_de_conclusao, -email, -nome, -idioma,
                -nome2, -o_cliente_que_esta_visitando_ja_e_um_revendedor_tee_jet,
                -territorio_da_tee_jet_br_onde_esta_sediado_o_cliente, -cidade,
                -telefone_e_nome_de_um_contato_na_empresa_revendedora_dealer,
                -quais_materiais_ferramentas_voce_apresentou_utilizou_para_o_cliente_durante_essa_visita,
                -inserir_sigla_do_modelo_da_ponta_de_pulverizacao_nome_do_fabricante_preco_encontrado_ex_grd120_025_hypro_r_37_50, 
                -potencial_do_cliente_como_influenciador_em_t_a_na_regiao,
                -conhecimento_tecnico_e_valorizacao_da_importancia_das_pontas_de_pulverizacao,
                -interesse_dos_gestores_e_funcionarios_da_revenda_pelas_pontas_tj,
                -chance_de_crescimento_da_marca_tee_jet_neste_cliente,
                -relevancia_deste_cliente_na_regiao_onde_esta_a_sediada_a_revenda,
                -probabilidade_do_cliente_efetivar_uma_compra_apos_essa_visita,
                -magnojet2, -tee_jet2, -hypro2, -jacto3, -magnojet3,
                -qual_o_nivel_de_satisfacao_do_cliente_apos_sua_visita,
                -insira_suas_sugestoes_e_comentarios_referentes_a_essa_pesquisa_de_mercado,
                -quanto_essa_pesquisa_contribuiu_para_sua_avaliacao_sobre_a_visita_ao_seu_cliente, 
                -tee_jet, -hypro, -jacto2) |> 
  rename(
    nome_da_revenda = nome_do_revendedor_que_esta_visitando_dealer_ex_mac_campo,
    estado = em_qual_estado_brasileiro_esta_sediada_a_empresa_visitada,
    marcas = quais_marcas_de_pontas_de_pulverizacao_a_revenda_mantem_em_estoque
  ) |> 
  pivot_longer(cols = txa:jmd, names_to = "pontas", values_to = "price") |> 
  mutate(price = str_remove(price, ">"),
         price = str_remove(price, "<"),
         price = str_replace(price, ",", "."),
         price = str_replace(price, ",", ".")) |> 
  filter(!is.na(price)) |> 
  mutate(pontas = str_replace(pontas, "_", "-"),
         pontas = str_to_upper(pontas)) |> 
  separate(price, into = c("min", "max"), sep = " a ") |> 
  mutate_at(c("min", "max"), as.double) |> 
  mutate(max = if_else(is.na(max), min, max)) |> 
  mutate(price = round((min + max)/ 2,2)) |> 
  dplyr::select(estado, nome_da_revenda, marcas, pontas, price) -> dados_22



dados_22 |> 
  left_join(group, by = c("pontas" = "family")) |> 
  mutate(ano = 2022) |> 
  distinct(estado, nome_da_revenda, pontas, price, .keep_all = TRUE) -> dados_2022



data_cb <- bind_rows(dados_2021, dados_2022)


