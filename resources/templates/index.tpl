<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Заполнятор</title>
    <link rel="stylesheet" href="/s/css/main.css" />

    <!-- DOM manipulation -->
    <script src="/s/js/thirdparty/jquery-1.7.1.min.js" />

    <script src="/s/js/thirdparty/knockout-2.0.0.js" />

    <!-- Utility library, Backbone dependency -->
    <script src="/s/js/thirdparty/underscore-min.js" />
    <!-- Loose MVC -->
    <script src="/s/js/thirdparty/backbone-min.js" />

    <!-- Knockback is a Knockout + Backbone glue -->
    <script src="/s/js/thirdparty/knockback-0.12.js" />

    <!-- Simple templates -->
    <script src="/s/js/thirdparty/mustache.js" />

    <script src="/s/js/metamodel.js" />
    <script src="/s/js/load-model.js" />
  </head>
  <body>
    <div id="sidebar">
      <div class="block" id="login">
        <ifLoggedIn>
          <div style="float:left;">
            <loggedInUser /><br />
          </div>
          <div style="float: right;">
            <a href="/logout/">
              <img src="/s/images/exit.png" />
            </a>
          </div>
          <div style="clear: both;" />
        </ifLoggedIn>
      </div>
      <div class="block" id="messages" />
      <div class="block" id="timeline" />
    </div>

    <div id="container" class="block">
      <h1>Заполнение формы «<span id="model-name" />»</h1>
      <div id="message" style="display: none;" />
      <form>
        <ul id="form" />
        <button class="btn" type="button"
                onClick="proceed();">Сохранить и начать новую</button>
        <button class="btn danger" type="button"
                style="float:right;"
                onClick="remove();">Удалить</button>
      </form>
    </div>

    <!-- Form field templates -->
    <script type="text/template" 
            class="field-template"
            id="textarea-field-template">
      <li class="{{ name }} item">
        <label>{{ label }}:<br />
          <textarea class="{{ name }} field" 
                    name="{{ name }}" 
                    rows="7" cols="70"
                    data-bind="value: {{ name }}"></textarea>
        </label>
      </li>
    </script>

    <script type="text/template"
            class="field-template"
            id="text-field-template">
      <li class="{{ name }} item">
        <label>{{ label }}:<br />
          <input type="text"
                 class="{{ name }} field"
                 name="{{ name }}"
                 size="70"
                 data-bind="value: {{ name }}, 
                            valueUpdate: 'afterkeydown'" />
        </label>
      </li>
    </script>

    <script type="text/template"
            class="field-template"
            id="select-field-template">
      <li class="{{ name }} item">
        <label>{{ label }}:<br />
          <select class="{{ name }} field"
                  name="{{ name }}"
                  data-bind="value: '{{ name }}',
                             valueUpdate: 'afterkeydown'">
            {{# choice }}
            <option value="{{.}}">{{.}}</option>
            {{/ choice }}
          </select>
        </label>
      </li>
    </script>

    <script type="text/template"
            class="field-template"
            id="checkbox-field-template">
      <li class="{{ name }} item">
        <label><input type="checkbox" 
                      class="{{ name }} field"
                      name="{{ name }}"
                      data-bind="value: {{ name }},
                                 valueUpdate: 'change'" />
          {{ label }}
        </label>
      </li>
    </script>

    <!-- Template for fields with unknown type -->
    <script type="text/template"
            class="field-template"
            id="unknown-field-template">
      <li class="{{ name }} item">
        {{ name }}
      </li>
    </script>

    <script type="text/template" id="timeline-item">
      <span class="btn{{# sel }} info{{/ sel }}"
            id="timeline-{{id}}"
            onClick="restore({{ id }});">{{ id }} </span>
    </script>

    <!-- WebSocket notifications -->
    <script type="text/template" id="message-create">
      <li style="color: green;">+{{ id }}</li>
    </script>

    <script type="text/template" id="message-delete">
      <li style="color: red;">-{{ id }}</li>
    </script>
  </body>
</html>
