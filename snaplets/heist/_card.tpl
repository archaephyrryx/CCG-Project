<form method="GET" action="/card">
  <table id="filter">
    <tr>
      <td>
	<div id="minmax">
	  <label for="powRange">Power</label>
	  <div id="powRange">
	    <input name="minPower" placeholder="Min" type="number" min="0" step="1" pattern="\d+"/>
	    to
	    <input name="maxPower" placeholder="Max" type="number" min=${powmin} step="1" pattern="\d+"/>
	  </div>
	  <label for="costRange">Cost</label>
	  <div id="costRange">
	    <input name="minPower" placeholder="Min" type="number" min="0" step="1" pattern="\d+"/>
	    to
	    <input name="maxPower" placeholder="Max" type="number" min=${costmin} step="1" pattern="\d+"/>
	  </div>
	  <label for="reqRange">Requirement</label>
	  <div id="reqRange">
	    <input name="minReq" placeholder="Min" type="number" min="0" step="1" pattern="\d+"/>
	    to
	    <input name="maxReq" placeholder="Max" type="number" min=${reqmin} step="1" pattern="\d+"/>
	  </div>
	</div>
      </td>
      <apply template="select-form" />
    </tr>
    <tr>
      <td></td>
      <td><input type="submit" value="Filter Cards" /> </td>
    </tr>
  </table>
</form>
