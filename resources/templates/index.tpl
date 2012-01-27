<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Заполнятор</title>

    <!-- DOM manipulation -->
    <script src="/resources/static/js/jquery-1.7.1.min.js" />

    <!-- Utility library, Backbone dependency -->
    <script src="/resources/static/js/underscore-min.js" />
    <!-- Loose MVC -->
    <script src="/resources/static/js/backbone-min.js" />

    <!-- Simple templates -->
    <script src="/resources/static/js/mustache.js" />

    <script src="/resources/static/js/metamodel.js" />
    <script src="/resources/static/js/models/scp.js" />
    <script src="/resources/static/js/run.js" />
  </head>
  <body>
    <div class="container">
      <h1>Заполнение формы</h1>
      <form>
        <ul id="form" />
        <input type="submit" value="Отправить" />
      </form>
    </div>

    <script type="text/template" id="text-field-template">
      <li>
        <label>{{ field.label }}
          <input type="text" name="{{ field.name }}" 
                 value="{{ value }}" />
        </label>
      </li>
    </script>

    <script type="text/template" id="choice-field-template">
      <li>
        <select name="{{ field.name }}">
          {{ #choice }}
          <option value="{{ value }}" 
                  {{ #selected }}selected{{ /selected }}>
            {{ value }}
          </option>
          {{ /choice }}
        </select>
      </li>
    </script>
  </body>
</html>
