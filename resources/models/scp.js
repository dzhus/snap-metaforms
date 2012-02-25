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
            "canRead": ["front"]
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
            "label": "Класс"
        },
        {
            "name": "jobsDone",
            "label": "Работы завершены",
            "type": "checkbox"
        },
        {
            "name": "conditions",
            "type": "textarea",
            "label": "Условия содержания"
        },
        {
            "name": "description",
            "type": "textarea",
            "label": "Описание"
        }
    ]
}
