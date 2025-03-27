from django.contrib import admin
from what_the_book.models import *


class BookAdmin(admin.ModelAdmin):
    list_display = ('Title', 'Author', 'AddedBy', 'AddedOn', 'Likes')


admin.site.register(User)
admin.site.register(Admin)
admin.site.register(Book)
admin.site.register(BookToRequest)
admin.site.register(Review)
