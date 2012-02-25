{
    "title": "SCP",
    "fields": [
        {
            "name": "code",
            "label": "Код"
        },
        {
            "name": "title",
            "label": "Название"
        },
        {
            "name": "foundAt",
            "label": "Место обнаружения"
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
