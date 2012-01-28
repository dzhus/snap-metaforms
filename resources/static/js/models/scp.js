({
    "name": "SCP",
    "fields": [
        {
            "name": "code",
            "type": fieldType.number,
            "label": "Код",
        },
        {
            "name": "class",
            "type": fieldType.choice,
            "default": "Евклид",
            "choice": ["Безопасный", "Евклид", "Кетер"],
            "label": "Класс"
        },
        {
            "name": "conditions",
            "type": fieldType.longText,
            "label": "Условия содержания"
        },
        {
            "name": "description",
            "type": fieldType.longText,
            "label": "Описание"
        }
    ]
})
