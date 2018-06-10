describe("cambore", () => {
  it("ukaraa", () => {
    cambore.ukaraaIO().should.be.eql(undefined)
  })

  it("kemeroon", () => {
    cambore.kemeroonIO(4).should.be.eql(undefined)
  })

  it("xinaiu", () => {
    cambore.xinaiuIO("una montaña").should.be.eql(undefined)
  })

  it("laimaDaraa", () => {
    cambore.laimaDaraaIO().should.be.eql("Y yo estoy volando dese el alto cielo")
  })

  it("volvioBelixaMelicaan", () => {
    cambore.volvioBelixaMelicaanIO().should.be.eql(false)
  })

  it("cantidadDeSupremos", () => {
    cambore.cantidadDeSupremosIO().should.be.eql(7)
  })
})
