<link rel="stylesheet" href="../css/try.css">
<script language="javascript" src="../js/trymulang.js"></script>


<button id="analyse-button" onclick="analyse()">Analyse</button>

<span class="examples-container">
  Examples: <select class="examples-select">
  <option selected="selected"></option>
  <option value="intransitive">With intransitive expectations</option>
  <option value="unscoped">With unscoped expectations</option>
  <option value="matchers">With expectations with matchers</option>
  <option value="custom">With custom expectations</option>
  <option value="signature">With signature analysis</option>
  <option value="broken">With broken input</option>
  <option value="AST">With AST as input</option>
  <option value="smellInclusion">With smell analysis, by inclusion</option>
  <option value="smellExclusion">With smell analysis, by exclusion</option>
  <option value="expressiveness">With expressiveness smells</option>
  <option value="typosSmells">With typos smells</option>
  <option value="intermediate">With intermediate language generation</option>
  <option value="normalization">With normalization options</option>
  <option value="testRunning">With test running</option>
</select>
</span>


<div id="try-container">
  <div id="jsoneditor"></div>
  <pre id="result"></pre>
</div>
