<dfForm action="/card">
  <dfChildErrorList />
  <div>
    <dfLabel ref="powRange">Power</dfLabel>
    <dfSubview ref="powRange">
      <apply template="minmax-form" />
    </dfSubview>
  </div>
  <div>
    <dfLabel ref="costRange">Cost</dfLabel>
    <dfSubview ref="costRange">
      <apply template="minmax-form" />
    </dfSubview>
  </div>
  <div>
    <dfLabel ref="reqRange">Requirement</dfLabel>
    <dfSubview ref="reqRange">
      <apply template="minmax-form" />
    </dfSubview>
  </div>
  <div>
    <dfSubview ref="selection">
      <apply template="select-form" />
    </dfSubView>
  </div>
  <div> <dfInputSubmit value="Submit" /> </div>
</dfForm>

