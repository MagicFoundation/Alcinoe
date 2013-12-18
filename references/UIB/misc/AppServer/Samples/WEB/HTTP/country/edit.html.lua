<html>
  <body>
    <form action="/country/edit" method="POST">
      <input type="text" name="formulaire.country" value="<%= country %>"/>
      <input type="text" name="formulaire.currency" value="<%= currency %>"/>
      <input type="submit"/>
    </form>
  </body>
</html>