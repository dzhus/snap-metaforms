<!DOCTYPE html>
<html>
  <head>
    <title>Snap web server</title>
    <link rel="stylesheet"
          href="http://twitter.github.com/bootstrap/1.4.0/bootstrap.min.css"
          />
    <meta charset="utf-8" />
  </head>
  <body>
    <div id="content">
      <h1>It works!</h1>
      <p>
        This is a simple demo page served using
        <a href="http://snapframework.com/docs/tutorials/heist">Heist</a>
        and the <a href="http://snapframework.com/">Snap</a> web framework.
      </p>
      <p>
        Echo test:
        <a href="/echo/cats">cats</a>
        <a href="/echo/dogs">dogs</a>
        <a href="/echo/fish">fish</a>
      </p>
      <table id="info">
        <tr>
          <td>Config generated at:</td>
          <td><start-time/></td>
        </tr>
        <tr>
          <td>Page generated at:</td>
          <td><current-time/></td>
        </tr>
      </table>
    </div>
  </body>
</html>
