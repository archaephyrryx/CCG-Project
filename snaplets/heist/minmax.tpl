<label for="${rangeName}"><rangeLabel /></label>
<div id="${rangeName}">
  <input name="min${rangeLabel}" placeholder="Min" type="number" min="0" step="1" pattern="\d+" val="${minval}"/>
  to
  <input name="max${rangeLabel}" placeholder="Max" type="number" min=${minval} step="1" pattern="\d+"/>
</div>
