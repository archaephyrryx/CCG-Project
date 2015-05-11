<apply template="filter">
  <bind tag="pagename">
    SnappleJack: Card
  </bind>
  <bind tag="filter">
    <dfForm action="/card">
      <apply template="minmax" />
      <apply template="select" />
      <div> <dfInputSubmit value="Submit" /> </div>
    </dfForm>
  </bind>
  <bind tag="result">
  <!-- <resolve>${card}</resolve> -->
  </bind>
</apply>
