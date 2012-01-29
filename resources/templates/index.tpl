<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Заполнятор</title>
    <link rel="stylesheet" href="/resources/static/css/main.css" />

    <!-- DOM manipulation -->
    <script src="/resources/static/js/jquery-1.7.1.min.js" />

    <!-- Utility library, Backbone dependency -->
    <script src="/resources/static/js/underscore-min.js" />
    <!-- Loose MVC -->
    <script src="/resources/static/js/backbone-min.js" />

    <!-- Simple templates -->
    <script src="/resources/static/js/mustache.js" />

    <script src="/resources/static/js/metamodel.js" />
    <script src="/resources/static/js/load-model.js" />
  </head>
  <body>
    <div class="container">
      <h1>Заполнение формы «<span id="model-name" />»</h1>
      <form>
        <ul id="form" />
        <input type="submit" value="Отправить" />
      </form>
    </div>

    <script type="text/template" id="longtext-field-template">
      <li>
        <label>{{ field.label }}:<br />
          <textarea name="{{ field.name }}" rows="7" cols="70">{{ value }}</textarea>
        </label>
      </li>
    </script>

    <script type="text/template" id="text-field-template">
      <li>
        <label>{{ field.label }}:<br />
          <input type="text" name="{{ field.name }}" 
                 value="{{ value }}" />
        </label>
      </li>
    </script>

    <script type="text/template" id="choice-field-template">
      <li>
        <label>{{ field.label }}:<br />
          <select name="{{ field.name }}">
            {{# choice }}
            <option value="{{ value }}" 
                    {{# selected }}selected{{/ selected }}>
              {{ value }}
            </option>
            {{/ choice }}
          </select>
        </label>
      </li>
    </script>
  </body>
</html>
