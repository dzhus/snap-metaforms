{
    "title": "SCP",
    "fields": [
        {
            "name": "code",
            "label": "Код",
            "canRead": ["front"]
        },
        {
            "name": "title",
            "label": "Название",
            "canWrite": true
        },
        {
            "name": "foundAt",
            "label": "Место обнаружения",
            "canRead": ["front"]
        },
        {
            "name": "dangerClass",
            "type": "select",
            "default": "Евклид",
            "choice": ["Безопасный", "Евклид", "Кетер"],
            "label": "Класс",
            "canRead": ["front"]
        },
        {
            "name": "jobsDone",
            "label": "Работы завершены",
            "type": "checkbox",
            "canRead": ["front"]
        },
        {
            "name": "conditions",
            "type": "textarea",
            "label": "Условия содержания",
            "canRead": ["front"]
        },
        {
            "name": "description",
            "type": "textarea",
            "label": "Описание",
            "canRead": ["front"]
        }
    ]
}
