<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Заполнятор</title>
    <link rel="stylesheet" href="/resources/static/css/main.css" />

    <!-- DOM manipulation -->
    <script src="/resources/static/js/thirdparty/jquery-1.7.1.min.js" />

    <!-- Utility library, Backbone dependency -->
    <script src="/resources/static/js/thirdparty/underscore-min.js" />
    <!-- Loose MVC -->
    <script src="/resources/static/js/thirdparty/backbone-min.js" />

    <!-- Simple templates -->
    <script src="/resources/static/js/thirdparty/mustache.js" />

    <script src="/resources/static/js/metamodel.js" />
    <script src="/resources/static/js/load-model.js" />
  </head>
  <body>
    <div id="container">
      <h1>Заполнение формы «<span id="model-name" />»</h1>
      <form>
        <ul id="form" />
        <div class="well">
        <button class="btn" type="button"
                onClick="FormModel.save();">Сохранить и начать новую</button>
        <button class="btn danger" type="button"
                style="float:right;">Удалить</button>
        </div>
      </form>
    </div>

    <script type="text/template" id="longtext-field-template">
      <li class="{{ field.name }} item">
        <label>{{ field.label }}:<br />
          <textarea class="{{ field.name }} field" 
                    name="{{ field.name }}" 
                    rows="7" cols="70">{{ value }}</textarea>
        </label>
      </li>
    </script>

    <script type="text/template" id="text-field-template">
      <li class="{{ field.name }} item">
        <label>{{ field.label }}:<br />
          <input type="text"
                 class="{{ field.name }} field"
                 name="{{ field.name }}"
                 size="70"
                 value="{{ value }}" />
        </label>
      </li>
    </script>

    <script type="text/template" id="choice-field-template">
      <li class="{{ field.name }} item">
        <label>{{ field.label }}:<br />
          <select class="{{ field.name }} field"
                  name="{{ field.name }}">
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
