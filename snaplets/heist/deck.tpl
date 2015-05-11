<apply template="filter">
  <bind tag="pagename">
    SnappleJack: Deck Builder
  </bind>
  <bind tag="filter">
    <dfForm action="/deck">
      <apply template="select"/>
      <div> <dfInputSubmit value="Submit" /> </div>
    </dfForm>
  </bind>
  <bind tag="result">
  <!-- <resolve>${card}</resolve> -->
  </bind>
</apply>
