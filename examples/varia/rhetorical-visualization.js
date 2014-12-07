dojo.require("dijit.Menu");
dojo.require("dojo.parser");

//pop-up Menu function: resource - DOJO homepage!
dojo.addOnLoad(function() 
{
    function doNothing() 
    {
	alert('not actually doing anything, just a test!');
    }
    
    var pMenu = new dijit.Menu({targetNodeIds:["menu_div"], id:"menu_div"});
    pMenu.addChild(new dijit.MenuItem({label:"Enabled Item", disabled:true}));
    pMenu.addChild(new dijit.MenuItem({label:"Disabled Item", disabled:true}));
    pMenu.addChild(new dijit.MenuItem({label:"Cut", onClick:doNothing}));
    pMenu.addChild(new dijit.MenuItem({label:"Copy", onClick:doNothing}));
    pMenu.addChild(new dijit.MenuItem({label:"Paste", onClick:doNothing}));
    
    pMenu.startup();
});

/* function to be loaded with the page, it simply adds another *helping* value to the class attribute of the elements currently having a header and a link as their class attribute values. Later on these new helping values are to be used in the javascript functions */

function first()
{
    $('.header').addClass('help');
    $('.link').addClass('help2');
}

/* function taking care of the plain view mode. It simply removes all the CSS-existing classes that emphasize certain rhetorical structures in a document. Furthermore it also hides some html code that is not part of the original xhtml document */ 
function plain()
{
   $('.help3').removeClass('hide');
   $('.help3').addClass('hidden');
   $('.nucleus').removeClass('marked');
   $('.omdoc-omtext').removeClass('markbackground');
   $('.omdoc-omgroup').removeClass('markborder');
   $('.help').removeClass('header');
   $('.help2').removeClass('link');
   $('.help2').removeClass('satellites');
   $('.help').addClass('hidden');
   $('.help2').addClass('hidden');
   $('.satellite').removeClass('marked2');
   $('.satellite').removeClass('marked');
}

/*function responsible for the apperience of the blocks view mode perspective. It adds some CSS-recognized classes to the omdoc-omtext and omdoc-omgroup elements to emphasize them. It removes all the other CSS-recognized classes that are related to other rhetorical structure elements*/
function blocks()
{
   $('.omdoc-omtext').addClass('markbackground');
   $('.nucleus').removeClass('marked');
   $('.omdoc-omgroup').addClass('markborder');
   $('.help').removeClass('header');
   $('.help').removeClass('hidden');
   $('.help').addClass('hdr');
   $('.help2').removeClass('link');
   $('.help2').removeClass('satellites');
   $('.help2').addClass('hidden');
   $('.satellite').removeClass('marked2');
   $('.satellite').removeClass('marked');
   
}

/*function responsible for the apperience of the relations view mode perspective. It adds some CSS-recognized classes to the nuclei elements to emphasize them. It removes all the other CSS-recognized classes that are related to other rhetorical structure elements*/
function relations()
{
    $('.nucleus').addClass('marked');
    $('.omdoc-omtext').removeClass('markbackground');
    $('.omdoc-omgroup').removeClass('markborder');
    $('.help').removeClass('header');
    $('.help').removeClass('hdr');
    $('.help').addClass('hidden');
    $('.help2').removeClass('link');
    $('.help2').removeClass('hidden');
    $('.help2').addClass('satellites');
}

/*emphasize the nuclues - by making it bold in this case*/
function high(nucleus)
{
    var nuc = nucleus.replace('#','');
    $('span[id='+nuc+']').addClass('pointout');
}

/*de-emphasize the nucleus - return normal by removing the CSS-recognized class*/
function low(nucleus)
{
    var nuc2 = nucleus.replace('#','');
    $('span[id='+nuc2+']').removeClass('pointout');
}

/*highlight all satellites connected to the nucleus (passed as a parameter) by adding a certain CSS-recognized class to all matching html elements*/
function getSatellites(nuclues)
{
    $('span[resource=' + nuclues + ']').addClass('marked2');
}

/*get all satellites connected to the nuclues, passes as a parameter, with a specific relation. highlight them by adding a certain CSS-recognized class to all matching html elements */
function getSpecificSatellite(nucleus, relation)
{
    $('span[resource='+nucleus+']').filter('span[rel="http://salt.semanticauthoring.org/onto/rhetorical-ontology#' + relation + '"]').addClass('marked2');    
}

/*return the satellites in their normal representation form by removing the CSS-recognizes classes from the all the 'satellite' html elements*/
function outSatellites()
{
    $('.satellite').removeClass('marked2');
    $('.satellite').removeClass('marked');
}