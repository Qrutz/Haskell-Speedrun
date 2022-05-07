'''create class that represents a school'''


class School:
    def __init__(self, name, address):
        self.name = name
        self.address = address
        self.students = []
        self.teachers = []

    def add_student(self, student):
        self.students.append(student)

    def add_teacher(self, teacher):
        self.teachers.append(teacher)

    def print_students(self):
        for student in self.students:
            print(student.name)

    def convertGradeToInt(self, grade):
        if grade == 'A':
            return 4
        elif grade == 'B':
            return 3
        elif grade == 'C':
            return 2
        elif grade == 'D':
            return 1

    def getAverageGrade(self):
        grades = []
        for student in self.students:
            grades.append(self.convertGradeToInt(student.grade))
        return sum(grades) / len(grades)


'''create class that represents a student'''


class Student:
    def __init__(self, name, age, grade):
        self.name = name
        self.age = age
        self.grade = grade


class Teacher:
    def __init__(self, name, age, salary):
        self.name = name
        self.age = age
        self.salary = salary


skool = School('Harvard', '123 Main St')
skool.add_student(Student('John', 20, 'A'))
skool.add_student(Student('Mary', 20, 'B'))

'''create example'''

'''print the average grade of the students in the school'''
print(skool.getAverageGrade())

'''commit this file to your github repo'''
