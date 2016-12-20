if (typeof jQuery === 'undefined') {
  throw new Error('Bootstrap\'s JavaScript requires jQuery')
}
else
{
  $(document).ready(function()
  {
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
