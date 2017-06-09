context("Comprobaciones de clase y dimensiones correctas")
library(pintamapas)
data(privacion_valencia)
data(carto_valencia)
test_that("pinta_mapas solo funciona con SpatialPolygonsDataFrame", {
  expect_error(pinta_mapas("a", 1:10, 1))
  expect_error(pinta_mapas(TRUE, 1:10, 1))
  expect_error(pinta_mapas(1:10, 1:10, 1))
})
test_that("pinta_mapas solo funciona con variables numéricas", {
  expect_error(pinta_mapas(carto_valencia, 1, 1))
  4
  expect_error(pinta_mapas(carto_valencia, 1:40000, 1))
})
test_that("pinta_mapas solo funciona con n_grupos numérico y longitud 1", {
  expect_error(pinta_mapas(carto_valencia, privacion_valencia, "a"))
  expect_error(pinta_mapas(carto_valencia, privacion_valencia, 1:10))
})

