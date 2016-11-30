if (typeof jQuery === 'undefined') {
  throw new Error('Bootstrap\'s JavaScript requires jQuery')
}
else
{
  $(document).ready(function()
  {
    //It must be appended to check whether user has mobile version or desktop
    var $el = $('<div id="pn-mobile-indicator"></div>');
    $('.footer').append($el);

    $('a[href*=#]').click(function() 
    {
      if (location.pathname.replace(/^\//,'') == this.pathname.replace(/^\//,'')
      && location.hostname == this.hostname) 
      {
        var $target = $(this.hash);
        $target = $target.length && $target
        || $('[name=' + this.hash.slice(1) +']');
        if ($target.length) 
        {   
          if( $('.header').hasClass('fixed') )
          {
            var targetOffset = $target.offset().top - $('.header').height()  - 20;  
          }
          else
          {
            var targetOffset = $target.offset().top - 90 - $('.header').height();
          }
          
          $('html,body')
          .animate({scrollTop: targetOffset}, 1000);
          return false;
        }
      }
    });

    var lastScrollTop = 0;

    $(window).bind('scroll', function()
    {
      console.log();

      if( $el.is(':visible'))
      {
        var barra = $(window).scrollTop();
        var posicion = barra * 0.5;
        var opacity = 0;

        lastScrollTop = barra;
        
        $('body').css({
          'background-position': 'center -' + ( posicion ) + 'px'
        });

        if ($(window).scrollTop() > 50) {
               $('.header').addClass('fixed');
               $('.pn-top').fadeIn();
        }
        else {
           $('.header').removeClass('fixed');
           $('.pn-top').fadeOut();
        } 

        console.log('JS scroll turned on');  
      }
      else
      {
        console.log('JS scroll turned off');  
      }
    });

    /* Show read more for seminars */
    $('.pn-readmore-introtext').click(function()
    {
      $this = $(this);

      $this.parent().find('.pn-readmore-fulltext').animate({
        height: "toggle"
      }, 1000 );
    }); 

    $('.pn-seminar .pn-title').click(function()
    {
      $this = $(this).closest(".pn-seminar");

      if( $this.hasClass( 'compact' ) )
      {
            $this.find('br').remove();
            $this.find('.pn-title, .pn-url').after('<br />');

            $this.find('.pn-abstract-bio').show();

            $this.removeClass( 'compact' );
      }
      else
      {
        $this.find('.pn-abstract-bio').hide();
        $this.find('br').remove();
        $this.find('.pn-name').after('<br />').before('<br />');
        $this.addClass( 'compact' );
      }
    });

    $(".finished .pn-title").click();
  });
}
