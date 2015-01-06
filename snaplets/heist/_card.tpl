<form method="post" action="/card">
  <table id="filter">
    <tr>
      <td>
	<div id="minmax">
	  <label for="powRange">Power</label>
	  <div id="powRange">
	    <input name="powmin" placeholder="Min" type="number" min="0" step="1" pattern="\d+"/>
	    to
	    <input name="powmax" placeholder="Max" type="number" min=${powmin} step="1" pattern="\d+"/>
	  </div>
	  <label for="costRange">Cost</label>
	  <div id="costRange">
	    <input name="costmin" placeholder="Min" type="number" min="0" step="1" pattern="\d+"/>
	    to
	    <input name="costmax" placeholder="Max" type="number" min=${costmin} step="1" pattern="\d+"/>
	  </div>
	  <label for="reqRange">Requirement</label>
	  <div id="reqRange">
	    <input name="reqmin" placeholder="Min" type="number" min="0" step="1" pattern="\d+"/>
	    to
	    <input name="reqmax" placeholder="Max" type="number" min=${reqmin} step="1" pattern="\d+"/>
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

