$(()=>{

});

function analyse(){
  const data = JSON.parse($("#spec").val());
  const result = mulang.analyse(data);
  $("#result").jsonViewer(result);
}
